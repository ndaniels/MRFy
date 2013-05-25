// -*- mode: c-mode; c-basic-offset: 4 -*-

#include <assert.h>
#include <float.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "model.h"
#include "viterbi.h"

const double NOMEMO = -1.0;
const double NEGLOGZERO = DBL_MAX;

static inline int resindex(AA residue) { return residue - 'A'; }

static inline Score min(Score x, Score y) { return x < y ? x : y; }

extern QuerySequence  input_query;
extern struct HMM    *input_hmm;

typedef int NodeCount;
typedef int ResidueCount;

typedef struct MemoTable {
    NodeCount hmmlen;
    ResidueCount querylen;
    Score scores[];  // array (hmmlen * querylen * NUMLABELS)
                     // INVARIANT during construction:
    // NOMEMO is a cell never reached
    // other value is the min cost to reach that cell
    // from cell 0/0/Mat using the paths traversed so far
} *MemoTable;

// #error ERROR ERROR -- THE TABLE IS TOO SMALL ALONG THE HMM DIMENSION AND THE 
// QUERY DIMENSION


static inline Score *
cell(MemoTable t, StateLabel s, NodeCount j, ResidueCount i)
{
    return &t->scores[s + NUMLABELS * (j + t->hmmlen * i)];
}

static MemoTable
memo_new(NodeCount j, ResidueCount i)
{
    int numscores = j * i * NUMLABELS;
    MemoTable t = malloc(sizeof(*t) + numscores * sizeof(t->scores[0]));
    assert(t);

    t->hmmlen = j;
    t->querylen = i;
    for (int i = 0; i < numscores; i++)
        t->scores[i] = NOMEMO;

    return t;
}

static void 
down_successor(MemoTable t, StateLabel s, NodeCount j, ResidueCount i, Score x)
{
    if (x > NEGLOGZERO)
        x = NEGLOGZERO;
    Score *succ = cell(t, s, j, i); // the successor
    if (*succ == NOMEMO || x < *succ)
        *succ = x;
}
        
static Score
run_forward(HMM hmm, QuerySequence query)
{
    MemoTable mt;
    StateLabel s;
    NodeCount j; // number of nodes consumed on path to this state
    ResidueCount i; // number of residues consumed on path to this state

    NodeCount num_normals = hmm->size - 1; // number of nodes with normal successors
    ResidueCount n = strlen(query);

    mt = memo_new(hmm->size + 1, n + 1);

    for (i = 0; i < n; i++)
        for (s = Mat; s < NUMLABELS; s++)
            *cell(mt, s, 0, i) = NEGLOGZERO;
    *cell(mt, Mat, 0, 0) = 0.0; // BEGIN node

    for (j = 0; j < num_normals; j++)
        for (i = 0; i < n; i++) {
            AA residue = query[i];
            Score here = *cell(mt, Mat, j, i);
            down_successor(mt, Ins, j, i+1, 
                           here + hmm->nodes[j+1].m_i +
                           hmm->nodes[j+1].m_emission[resindex(residue)]);
            down_successor(mt, Mat, j+1, i+1, 
                           here + hmm->nodes[j].m_m +
                           hmm->nodes[j].m_emission[resindex(residue)]);
            down_successor(mt, Del, j+1, i, 
                           here + hmm->nodes[j].m_d);

            here = *cell(mt, Ins, j, i);
            down_successor(mt, Ins, j, i+1, 
                           here + hmm->nodes[j+1].i_i +
                           hmm->nodes[j+1].i_emission[resindex(residue)]);
            down_successor(mt, Mat, j+1, i+1, 
                           here + hmm->nodes[j].i_m +
                           hmm->nodes[j].m_emission[resindex(residue)]);

            residue = query[i+1];
            here = *cell(mt, Del, j, i);
            down_successor(mt, Mat, j+1, i+1, 
                           here + hmm->nodes[j].d_m +
                           hmm->nodes[j].m_emission[resindex(residue)]);
            down_successor(mt, Del, j+1, i, 
                           here + hmm->nodes[j].d_d);
        }

    /* Score here = *cell(mt, Mat, num_normals, n); */
    /* down_successor(mt, Mat, num_normals+1, n, */
                   /* here + hmm->nodes[num_normals].m_m); */
    /*  */
    /* here = *cell(mt, Ins, num_normals, n); */
    /* down_successor(mt, Ins, num_normals+1, n, */
                   /* here + hmm->nodes[num_normals].i_m); */
    /*  */
    /* here = *cell(mt, Del, num_normals, n); */
    /* down_successor(mt, Del, num_normals+1, n, */
                   /* here + hmm->nodes[num_normals].d_m); */

    // It seems like without this loop, not all entries in the DP table will
    // get filled. But maybe they don't need to be. Blech. -- Zombie Andrew
    for (i = 0; i < n; i++) {
        Score here = *cell(mt, Mat, num_normals, i);
        down_successor(mt, Mat, num_normals+1, i,
                       here + hmm->nodes[num_normals].m_m);
        
        here = *cell(mt, Ins, num_normals, i);
        down_successor(mt, Ins, num_normals+1, i,
                       here + hmm->nodes[num_normals].i_m);
        
        here = *cell(mt, Del, num_normals, i);
        down_successor(mt, Del, num_normals+1, i,
                       here + hmm->nodes[num_normals].d_m);
    }
    
    // the answer lies in *cell(t, Mat, hmm->size, n)
    Score answer = *cell(mt, Mat, num_normals, n-1);
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
    mt = memo_new(hmmlen + 1, querylen + 1);

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
main()
{
    (void) viterbi_dp;
    (void) viterbi_memo;
    (void) vee_memo;

    Score score;

    hmm_print(input_hmm);

    score = viterbi_memo(input_hmm, input_query);
    printf("\nMemo Score: %f\n", score);

    score = viterbi_dp(input_hmm, input_query);
    printf("\nDP Score:   %f\n", score);

    return 0;
}
