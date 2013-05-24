// -*- mode: c-mode; c-basic-offset: 4 -*-

#include <assert.h>
#include <float.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "model.h"
#include "viterbi.h"

const double NOMEMO = -1.0;
const double NEGLOGZERO = 10e1024;

static inline int res_number(AA residue) { return residue - 'A'; }

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

#error ERROR ERROR -- THE TABLE IS TOO SMALL ALONG THE HMM DIMENSION AND THE QUERY DIMENSION


static inline Score *
cell(MemoTable t, StateLabel s, NodeCount j, ResidueCount i) {
  return s + NUMLABELS * (j + t->hmmlen * i);
}

static MemoTable memo_new(NodeCount j, ResidueCount i) {
  int numscores = j * i * NUMLABELS;
  MemoTable t = malloc(sizeof *t + numscores * sizeof(t->scores[0]));
  assert(t);
  for (int i = 0; i < numscores; i++)
    t->scores[i] = NOMEMO;
}

static void 
down_successor(MemoTable t, StateLabel s, NodeCount j, ResidueCount i, Score x) {
    if (x > NEGLOGZERO)
        x = NEGLOGZERO;
    Score *succ = cell(t, s, j, i); // the successor
    Score old = *succ;
    if (old == NOMEMO)
        *succ = x;
    else if (x < old)
        *succ = x;
}
        
    

static run_forward(MemoTable t, HMM hmm, QuerySequence query) {
    cell(t, Mat, 0, 0) = 0.0; // BEGIN node


    NodeCount j; // number of nodes consumed on path to this state
    ResidueCount i; // number of residues consumed on path to this state
    StateLabel s; // label of current state

    NodeCount num_normals = hmm->size; // number of nodes with normal successors
    ResidueCount n = query->size;

    for (j = 0; j < num_normals; j++)
        for (i = 0; i < n; i++) {
            AA residue = query[i];
            Score here = *cell(t, j, i, Mat);
            down_successor(t, Ins, j, i+1, 
                           here + hmm->nodes[j].m_i +
                           hmm->nodes[j].m_emission[resindex(residue)]);
            down_successor(t, Mat, j+1, i+1, 
                           here + hmm->nodes[j].m_m +
                           hmm->nodes[j+1].m_emission[resindex(residue)]);
            down_successor(t, Del, j+1, i, 
                           here + hmm->nodes[j].m_d);

            here = *cell(t, j, i, Ins);
            down_successor(t, Ins, j, i+1, 
                           here + hmm->nodes[j].i_i +
                           hmm->nodes[j].i_emission[resindex(residue)]);
            down_successor(t, Mat, j+1, i+1, 
                           here + hmm->nodes[j].i_m +
                           hmm->nodes[j+1].m_emission[resindex(residue)]);

            here = *cell(t, j, i, Del);
            down_successor(t, Mat, j+1, i+1, 
                           here + hmm->nodes[j].d_m +
                           hmm->nodes[j+1].m_emission[resindex(residue)]);
            down_successor(t, Del, j+1, i, 
                           here + hmm->nodes[j].d_d);
        }
    Score here = *cell(t, num_normals-1, n, Mat);
    down_successor(t, Mat, num_normals, n, Mat,
                   here + hmm->nodes[num_normals-1].m_m);
    
    here = *cell(t, num_normals-1, n, Ins);
    down_successor(t, Ins, num_normals, n, Mat,
                   here + hmm->nodes[num_normals-1].i_m);
    
    here = *cell(t, num_normals-1, n, Del);
    down_successor(t, Del, num_normals, n, Mat,
                   here + hmm->nodes[num_normals-1].d_m);
    
    // the answer lies in *cell(t, num_normals, n, Mat)
}

static struct ScoredPath *
vee(struct HMM *hmm, QuerySequence query,
    enum StateLabel stateRight, int32_t nc, int32_t rc,
    struct MemoTable *mt);

static struct ScoredPath *
vee_memo(struct HMM *hmm, QuerySequence query,
         enum StateLabel stateRight, int32_t nc, int32_t rc,
         struct MemoTable *mt);

static struct ScoredPath *
vee_score(enum StateLabel stateRight,
          struct HMM *hmm, QuerySequence query,
          enum StateLabel state, int32_t nc, int32_t rc,
          struct MemoTable *mt);

static struct MemoTable *
memo_table_new(int32_t hmmlen, int32_t querylen);

static void
memo_table_free(struct MemoTable * mt);

Score
scored_path_combine(struct ScoredPath *sp, enum StateLabel state, Score score,
                struct ScoredPath *result)
{
    int32_t i;

    result->path_len = sp->path_len;
    for (i = 0; i < sp->path_len; i++)
        result->path[i] = sp->path[i];

    result->score = score;
    result->path[result->path_len++] = state;
    return score;
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

void
scored_path_print(struct ScoredPath *sp)
{
    int32_t i;

    printf("States: %d\n", sp->path_len);
    printf("Score: %f\n", sp->score);

    for (i = 0; i < sp->path_len; i++)
        printf("%s ", label_names[sp->path[i]]);
    printf("\n");
}

struct ScoredPath *
viterbi(struct HMM *hmm, QuerySequence query)
{
    struct ScoredPath *result, *answer;
    struct MemoTable *mt;
    int32_t hmmlen, querylen;

    hmmlen = hmm->size;
    querylen = strlen(query);
    mt = memo_table_new(hmmlen + 1, querylen + 1);

    result = malloc(sizeof(*result));
    assert(result != NULL);

    answer = vee(hmm, query, MAT, hmmlen, querylen, mt);
    result = memcpy(result, answer);

    memo_table_free(mt);
    return result;
}

static struct ScoredPath *
vee(struct HMM *hmm, QuerySequence query,
    enum StateLabel stateRight, int32_t nc, int32_t rc,
    struct MemoTable *mt)
{
    struct ScoredPath *msp, *isp, *dsp;

    /* msp, isp and dsp correspond to the scored state paths
     * of each possible state transition to `stateRight`.
     *
     * `result` will be filled with the best scored state
     * path of the three.
     */
    msp.path_len = 0;
    msp.score = 0.0;
    isp.path_len = 0;
    isp.score = 0.0;
    dsp.path_len = 0;
    dsp.score = 0.0;

    if (MAT == stateRight && 0 == nc && 0 == rc) {
        fprintf(stderr, "reachable unreachable state Mat/0/0\n");
        exit(1);
    } else if (INS == stateRight && 0 == nc) { /* intoInsZero */
        if (0 == rc)
            return hmm->nodes[0].m_i;
        else {
            isp = vee_score(INS, hmm, query, INS, nc, rc, mt);
            return scored_path_combine(&isp, INS, is, mt[INS][nc][rc]);
        }
    } else if (MAT == stateRight && 1 == nc) { /* intoMatOne */
        if (0 == rc)
            return hmm->nodes[0].m_m;
        else {
            is = vee_score(MAT, hmm, query, INS, nc, rc, mt, &isp);
            ds = vee_score(MAT, hmm, query, DEL, nc, rc, mt, &dsp);

            if (is <= ds)
                return scored_path_combine(&isp, INS, is, result);
            else
                return scored_path_combine(&dsp, DEL, ds, result);
        }
    } else if (DEL == stateRight && 1 == nc) { /* intoDelOne */
        if (0 == rc)
            return hmm->nodes[0].m_d;
        else
            return NEGLOGZERO;
    } else {
        switch (stateRight) {
        case MAT:
            ms = vee_score(MAT, hmm, query, MAT, nc, rc, mt, &msp);
            is = vee_score(MAT, hmm, query, INS, nc, rc, mt, &isp);
            ds = vee_score(MAT, hmm, query, DEL, nc, rc, mt, &dsp);

            if (ms <= is && ms <= ds)
                return scored_path_combine(&msp, MAT, ms, result);
            else if (is <= ds)
                return scored_path_combine(&isp, INS, is, result);
            else
                return scored_path_combine(&dsp, DEL, ds, result);
            break;
        case INS:
            ms = vee_score(INS, hmm, query, MAT, nc, rc, mt, &msp);
            is = vee_score(INS, hmm, query, INS, nc, rc, mt, &isp);

            if (ms <= is)
                return scored_path_combine(&msp, MAT, ms, result);
            else
                return scored_path_combine(&isp, INS, is, result);
            break;
        case DEL:
            ms = vee_score(DEL, hmm, query, MAT, nc, rc, mt, &msp);
            ds = vee_score(DEL, hmm, query, DEL, nc, rc, mt, &dsp);

            if (ms <= ds)
                return scored_path_combine(&msp, MAT, ms, result);
            else
                return scored_path_combine(&dsp, DEL, ds, result);
            break;
        default:
            assert(false);
            break;
        }
    }

    assert(false);
}

static struct ScoredPath *
veexxx(struct HMM *hmm, QuerySequence query,
    enum StateLabel stateRight, int32_t nc, int32_t rc,
    struct MemoTable *mt)
{
    struct ScoredPath *msp, *isp, *dsp;

    /* msp, isp and dsp correspond to the scored state paths
     * of each possible state transition to `stateRight`.
     *
     * `result` will be filled with the best scored state
     * path of the three.
     */
    NodeCount j = nc;
    ResidueCount i = rc;

    if (MAT == stateRight && 0 == nc && 0 == rc) {
        fprintf(stderr, "reachable unreachable state Mat/0/0\n");
        exit(1);
    } else if (INS == stateRight && 0 == nc) { /* intoInsZero */
        if (0 == rc)
            return hmm->nodes[0].m_i;
        else {
            isp = vee_score(INS, hmm, query, INS, nc, rc, mt);
            return scored_path_combine(&isp, INS, is, mt[INS][nc][rc]);
        }
    } else if (MAT == stateRight && 1 == nc) { /* intoMatOne */
        if (0 == rc)
            return hmm->nodes[0].m_m;
        else {
            is = vee_score(MAT, hmm, query, INS, nc, rc, mt, &isp);
            ds = vee_score(MAT, hmm, query, DEL, nc, rc, mt, &dsp);

            if (is <= ds)
                return scored_path_combine(&isp, INS, is, result);
            else
                return scored_path_combine(&dsp, DEL, ds, result);
        }
    } else if (DEL == stateRight && 1 == nc) { /* intoDelOne */
        if (0 == rc)
            return hmm->nodes[0].m_d;
        else
            return NEGLOGZERO;
    } else {
        switch (stateRight) {
        case MAT:
            Score ms = vee_score(MAT, hmm, query, MAT, j, i, mt);
            Score is = vee_score(MAT, hmm, query, INS, j, i, mt);
            Score ds = vee_score(MAT, hmm, query, DEL, nc, rc, mt);

            return min (NEGLOGZERO, min (ms, min (is, ds)));
        case INS:
            Score ms = vee_score(MAT, hmm, query, MAT, nc, rc, mt, &msp);
            Score is = vee_score(MAT, hmm, query, INS, nc, rc, mt, &isp);
            Score ds = vee_score(MAT, hmm, query, DEL, nc, rc, mt, &dsp);

            return min (NEGLOGZERO, min (ms, min (is, ds)));
            break;

            if (ms <= is)
                return scored_path_combine(&msp, MAT, ms, result);
            else
                return scored_path_combine(&isp, INS, is, result);
        case DEL:
            ms = vee_score(DEL, hmm, query, MAT, nc, rc, mt, &msp);
            ds = vee_score(DEL, hmm, query, DEL, nc, rc, mt, &dsp);

            if (ms <= ds)
                return scored_path_combine(&msp, MAT, ms, result);
            else
                return scored_path_combine(&dsp, DEL, ds, result);
        default:
            assert(false);
            break;
        }
    }

    assert(false);
}

static Score
vee_memo(struct HMM *hmm, QuerySequence query,
         enum StateLabel stateRight, int32_t nc, int32_t rc,
         struct MemoTable *mt, struct ScoredPath *result)
{
    Score *here = cell(mt, stateRight, nc, rc);
    if (*here == NOMEMO)
        *s = vee(hmm, query, stateRight, nc, rc, mt, result);
    return *s;
}

static Score
vee_score(enum StateLabel stateRight,
          struct HMM *hmm, QuerySequence query,
          enum StateLabel state, int32_t nc, int32_t rc,
          struct MemoTable *mt, struct ScoredPath *result)
{
    if (stateRight != INS)
        nc--;
    if (state != DEL)
        rc--; 
    if (nc < 0 || rc < 0)
        return NEGLOGZERO; /* i.e., `internal []` */

    Score s = 0.0;
    switch (state) {
    case MAT:
        switch (stateRight) {
        case MAT:
            s = hmm->nodes[nc].m_m;
            break;
        case INS:
            s = hmm->nodes[nc].m_i;
            break;
        case DEL:
            s = hmm->nodes[nc].m_d;
            break;
        default:
            assert(false);
            break;
        }
        s = score_add(s, hmm->nodes[nc].m_emission[RESIND(rc)]);
        break;
    case INS:
        switch (stateRight) {
        case MAT:
            s = hmm->nodes[nc].i_m;
            break;
        case INS:
            s = hmm->nodes[nc].i_i;
            break;
        case DEL:
            assert(false);
            break;
        default:
            assert(false);
            break;
        }
        s = score_add(s, hmm->nodes[nc].i_emission[RESIND(rc)]);
        break;
    case DEL:
        switch (stateRight) {
        case MAT:
            s = hmm->nodes[nc].d_m;
            break;
        case INS:
            assert(false);
            break;
        case DEL:
            s = hmm->nodes[nc].d_d;
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

    return score_add(s, vee(hmm, query, state, nc, rc, mt, result));
}

static struct MemoTable *
memo_table_new(int32_t hmmlen, int32_t querylen)
{
    struct MemoTable *mt;
    Score ***mts;
    struct ScoredPath *sp;
    int32_t i, j, k;

    mts = malloc(sizeof(*mts) * NUMLABELS);
    assert(mts != NULL);

    for (i = 0; i < NUMLABELS; i++) {
        mts[i] = malloc(sizeof(*mts[i]) * hmmlen);
        assert(mts[i] != NULL);
        for (j = 0; j < hmmlen; j++) {
            mts[i][j] = malloc(sizeof(*mts[i][j]) * querylen);
            assert(mts[i][j] != NULL);

            for (k = 0; k < querylen; k++) {
                sp = malloc(sizeof(*sp));
                assert(sp != NULL);

                sp->score = NOMEMO;
                sp->path_len = 0;
                mts[i][j][k] = sp;
            }
        }
    }

    mt = malloc(sizeof(*mt));
    assert(mt != NULL);

    mt->hmmlen = hmmlen;
    mt->querylen = querylen;
    mt->scores = mts;
    return mt;
}

static void
memo_table_free(struct MemoTable * mt)
{
    int32_t i, j, k;

    for (i = 0; i < NUMLABELS; i++) {
        for (j = 0; j < mt->hmmlen; j++)
            for (k = 0; k < querylen; k++)
                free(mt->scores[i][j][k]);
            free(mt->scores[i][j]);
        free(mt->scores[i]);
    }
    free(mt->scores);
    free(mt);
}

int
main()
{
    (void) vee_memo;
    struct ScoredPath *result;

    result = viterbi(input_hmm, input_query);

    hmm_print(input_hmm);
    printf("\n");
    scored_path_print(result);

    free(result);
    return 0;
}
