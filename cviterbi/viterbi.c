// -*- mode: c-mode; c-basic-offset: 4 -*-

#include <assert.h>
#include <float.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "model.h"
#include "viterbi.h"
#include "input_hmm.h"

const double NOMEMO = -1.0;
const double NEGLOGZERO = DBL_MAX;

static inline int resindex(AA residue) { return residue - 'A'; }

static inline Score min(Score x, Score y) { return x < y ? x : y; }
static inline Score max(Score x, Score y) { return x > y ? x : y; }

extern QuerySequence  *input_query;


typedef int NodeCount;
typedef int ResidueCount;

typedef struct MemoTable {
    NodeCount hmmlenplus;
    ResidueCount querylenplus;
    Score scores[];  // array [NUMLABELS * hmmlenplus * querylenplus]
                     // INVARIANT during construction:
    // NOMEMO is a cell never reached
    // other value is the min cost to reach that cell
    // from cell Mat/0/0 using the paths traversed so far.
    // Residues are numbered from 0.  The score for cell s/j/0
    // includes no emission scores.  The score for cell s/j/i
    // includes the score for the emission of residue i-1 at state s/j.
    // (Index i is the total number of residues emitted at that point.)
} *MemoTable;



static inline Score *
cell(MemoTable t, StateLabel s, NodeCount j, ResidueCount i)
{
    return &t->scores[s + NUMLABELS * (j + t->hmmlenplus * i)];
}

static MemoTable
memo_new(NodeCount j, ResidueCount i, Score init)
{
    int numscores = (j+1) * (i+1) * NUMLABELS;
    MemoTable t = malloc(sizeof(*t) + numscores * sizeof(t->scores[0]));
    assert(t);

    t->hmmlenplus = j+1;
    t->querylenplus = i+1;
    for (int i = 0; i < numscores; i++)
        t->scores[i] = init;
    return t;
}

static void 
down_successor(MemoTable t, StateLabel s, NodeCount j, ResidueCount i, Score x)
{
    if (x > NEGLOGZERO)
        x = NEGLOGZERO;
    Score *succ = cell(t, s, j, i); // the successor
    if (x < *succ)
        *succ = x;
}
        
static Score
run_forward(HMM hmm, QuerySequence query)
{
    MemoTable mt;
    NodeCount j; // number of nodes consumed on path to this state
    ResidueCount i; // number of residues consumed on path to this state

    NodeCount num_normals = hmm->size - 1; // number of nodes with normal successors
    ResidueCount n = strlen(query);

    mt = memo_new(hmm->size + 1, n + 1, NEGLOGZERO);
    Node nodes = &hmm->nodes[0];

    *cell(mt, Mat, 0, 0) = 0.0; // BEGIN node

    
    for (j = 0; j < num_normals; j++)
        for (i = 0; i < n; i++) {
            // set scores for three states s/j/i
            AA residue = query[i];
            int resind = resindex(residue);
            Score m_emission =
                j + 1 < num_normals ? nodes[j+1].m_emission[resind] : 0.0;
                // the end state (node num_normals) does not emit
            Score i_emission = nodes[j].i_emission[resind];

            Score here;

            here = *cell(mt, Mat, j, i);
            down_successor(mt, Ins, j,   i+1, here + nodes[j].m_i + i_emission);
            down_successor(mt, Mat, j+1, i+1, here + nodes[j].m_m + m_emission);
            down_successor(mt, Del, j+1, i,   here + nodes[j].m_d);

            here = *cell(mt, Ins, j, i);
            down_successor(mt, Ins, j,   i+1, here + nodes[j].i_i + i_emission);
            down_successor(mt, Mat, j+1, i+1, here + nodes[j].i_m + m_emission);

            here = *cell(mt, Del, j, i);
            down_successor(mt, Mat, j+1, i+1, here + nodes[j].d_m + m_emission);
            down_successor(mt, Del, j+1, i,   here + nodes[j].d_d);

        }

    /* i = n; */
    /* j = hmm->size; */
    /* while (i >= 0 && j >= 0) { */
        /* Score ms = *cell(mt, Mat, j, i); */
        /* Score is = *cell(mt, Ins, j, i); */
        /* Score ds = *cell(mt, Del, j, i); */
/*  */
        /* if (ms <= is && ms <= ds) { */
            /* printf("Mat "); */
            /* i--; */
            /* j--; */
        /* } else if (is <= ds) { */
            /* printf("Ins "); */
            /* i--; */
        /* } else { */
            /* printf("Del "); */
            /* j--; */
        /* } */
    /* } */
    
    // the answer lies in *cell(t, Mat, hmm->size, n)
    Score answer = *cell(mt, Mat, num_normals, n);
    free(mt);
    return answer;
}

static Score
vee(struct HMM *hmm, QuerySequence query,
    StateLabel stateRight, NodeCount j, ResidueCount i,
    struct MemoTable *mt);

static Score
vee_memo(struct HMM *hmm, QuerySequence query,
         StateLabel stateRight, NodeCount j, ResidueCount i,
         struct MemoTable *mt)
{
    Score *here = cell(mt, stateRight, j, i);
    if (*here == NOMEMO)
        *here = vee(hmm, query, stateRight, j, i, mt);
    return *here;
}

static Score
vee_score(StateLabel stateRight,
          struct HMM *hmm, QuerySequence query,
          StateLabel state, NodeCount j, ResidueCount i,
          struct MemoTable *mt)
{
    if (stateRight != Ins)
        j--;
    if (state != Del)
        i--; 
    if (j < 0 || i < 0)
        return NEGLOGZERO; /* i.e., `internal []` */

    Score s = 0.0;
    switch (state) {
    case Mat:
        switch (stateRight) {
        case Mat:
            s = hmm->nodes[j].m_m;
            break;
        case Ins:
            s = hmm->nodes[j].m_i;
            break;
        case Del:
            s = hmm->nodes[j].m_d;
            break;
        default:
            assert(false);
            break;
        }
        s = score_add(s, hmm->nodes[j].m_emission[resindex(i)]);
        break;
    case Ins:
        switch (stateRight) {
        case Mat:
            s = hmm->nodes[j].i_m;
            break;
        case Ins:
            s = hmm->nodes[j].i_i;
            break;
        case Del:
            assert(false);
            break;
        default:
            assert(false);
            break;
        }
        s = score_add(s, hmm->nodes[j].i_emission[resindex(i)]);
        break;
    case Del:
        switch (stateRight) {
        case Mat:
            s = hmm->nodes[j].d_m;
            break;
        case Ins:
            assert(false);
            break;
        case Del:
            s = hmm->nodes[j].d_d;
            break;
        default:
            assert(false);
            break;
        }
        break;
    default:
        assert(false);
        break;
    }

    return score_add(s, vee_memo(hmm, query, state, j, i, mt));
}

static Score
vee(struct HMM *hmm, QuerySequence query,
    StateLabel stateRight, NodeCount j, ResidueCount i,
    struct MemoTable *mt)
{
    if (Mat == stateRight && 0 == j && 0 == i) {
        fprintf(stderr, "reachable unreachable state Mat/0/0\n");
        exit(1);
    } else if (Ins == stateRight && 0 == j) { /* intoInsZero */
        if (0 == i)
            return hmm->nodes[0].m_i;
        else {
            return min(NEGLOGZERO, vee_score(Ins, hmm, query, Ins, j, i, mt));
        }
    } else if (Mat == stateRight && 1 == j) { /* intoMatOne */
        if (0 == i)
            return hmm->nodes[0].m_m;
        else {
            Score is = vee_score(Mat, hmm, query, Ins, j, i, mt);
            Score ds = vee_score(Mat, hmm, query, Del, j, i, mt);

            return min(NEGLOGZERO, min(is, ds));
        }
    } else if (Del == stateRight && 1 == j) { /* intoDelOne */
        if (0 == i)
            return hmm->nodes[0].m_d;
        else
            return NEGLOGZERO;
    } else {
        Score ms, is, ds;

        switch (stateRight) {
        case Mat:
            ms = vee_score(Mat, hmm, query, Mat, j, i, mt);
            is = vee_score(Mat, hmm, query, Ins, j, i, mt);
            ds = vee_score(Mat, hmm, query, Del, j, i, mt);

            return min(NEGLOGZERO, min(ms, min(is, ds)));
        case Ins:
            ms = vee_score(Ins, hmm, query, Mat, j, i, mt);
            is = vee_score(Ins, hmm, query, Ins, j, i, mt);

            return min(NEGLOGZERO, min(ms, is));
        case Del:
            ms = vee_score(Del, hmm, query, Mat, j, i, mt);
            ds = vee_score(Del, hmm, query, Del, j, i, mt);

            return min(NEGLOGZERO, min(ms, ds));
        default:
            assert(false);
            break;
        }
    }

    assert(false);
}

Score
score_add(Score s1, Score s2)
{
    Score sum;

    if (s1 == NEGLOGZERO || s2 == NEGLOGZERO)
        return NEGLOGZERO;
    sum = s1 + s2;
    if (NEGLOGZERO <= sum)
        return NEGLOGZERO;
    return sum;
}

Score
viterbi_memo(struct HMM *hmm, QuerySequence query)
{
    Score answer;
    MemoTable mt;
    NodeCount hmmlen;
    ResidueCount querylen;

    hmmlen = hmm->size;
    querylen = strlen(query);
    mt = memo_new(hmmlen + 1, querylen + 1, false);

    answer = vee(hmm, query, Mat, hmmlen, querylen, mt);

    free(mt);
    return answer;
}

Score
viterbi_dp(struct HMM *hmm, QuerySequence query)
{
    return run_forward(hmm, query);
}

int
main(int argc, char **argv)
{
    (void) viterbi_memo;
    (void) vee_memo;
    int passes = 1;
    QuerySequence q;

    Score score = 0.0;
    if (argc == 2)
        passes = atoi(argv[1]);

    /* hmm_print(input_hmms); */

    /* score = viterbi_memo(input_hmms, input_query); */
    /* printf("\nMemo Score: %f\n", score); */

    printf("%d\n", num_models);

    for (int j = 0; j < num_models; j++) {
        QuerySequence *qs = input_query;
        while (NULL != (q = *qs++)) {
            for (int i = 0; i < passes; i++)
                score = viterbi_dp(&input_hmms[j], q);
            printf("Model %d (%3d nodes) query %d (%4d residues) DP Score:  %9.4f\n",
                   j, input_hmms[j].size, qs-input_query, strlen(q), score);
        }
    }

    return 0;
}
