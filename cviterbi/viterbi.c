#include <assert.h>
#include <float.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "model.h"
#include "viterbi.h"

#define NOMEMO -1
#define NEGLOGZERO FLT_MAX
#define INFINITY 0.0

#define RESIND(ri) (ri) - 'A'

extern QuerySequence  input_query;
extern struct HMM    *input_hmm;

struct MemoTable {
    Score ***scores;
    int32_t hmmlen;
    int32_t querylen;
};

static Score
vee(struct HMM *hmm, QuerySequence query,
    enum StateLabel stateRight, int32_t nc, int32_t rc,
    struct MemoTable *mt, struct ScoredPath *result);

static Score
vee_memo(struct HMM *hmm, QuerySequence query,
         enum StateLabel stateRight, int32_t nc, int32_t rc,
         struct MemoTable *mt, struct ScoredPath *result);

static Score
vee_score(enum StateLabel stateRight,
          struct HMM *hmm, QuerySequence query,
          enum StateLabel state, int32_t nc, int32_t rc,
          struct MemoTable *mt, struct ScoredPath *result);

static struct MemoTable *
memo_table_new(int32_t hmmlen, int32_t querylen);

static void
memo_table_free(struct MemoTable * mt);

Score
scored_path_add(struct ScoredPath *sp, enum StateLabel state, Score score,
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

void
scored_path_print(struct ScoredPath *sp)
{
    int32_t i;

    printf("States: %d\n", sp->path_len);
    printf("Score: %f\n", sp->score);

    for (i = 0; i < sp->path_len; i++)
        switch (sp->path[i]) {
        case MAT:
            printf("Mat ");
            break;
        case INS:
            printf("Ins ");
            break;
        case DEL:
            printf("Del ");
            break;
        default:
            assert(false);
            break;
        }
    printf("\n");
}

struct ScoredPath *
viterbi(struct HMM *hmm, QuerySequence query)
{
    struct ScoredPath *result;
    struct MemoTable *mt;
    int32_t hmmlen, querylen;

    hmmlen = hmm->size;
    querylen = strlen(query);
    mt = memo_table_new(hmmlen + 1, querylen + 1);

    result = malloc(sizeof(*result));
    assert(result != NULL);

    result->score = 0.0;
    result->path_len = 0;

    vee(hmm, query, MAT, hmmlen, querylen, mt, result);

    memo_table_free(mt);
    return result;
}

static Score
vee(struct HMM *hmm, QuerySequence query,
    enum StateLabel stateRight, int32_t nc, int32_t rc,
    struct MemoTable *mt, struct ScoredPath *result)
{
    Score ms, is, ds;
    struct ScoredPath msp, isp, dsp;

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
            is = vee_score(INS, hmm, query, INS, nc, rc, mt, &isp);
            return scored_path_add(&isp, INS, is, result);
        }
    } else if (MAT == stateRight && 1 == nc) { /* intoMatOne */
        if (0 == rc)
            return hmm->nodes[0].m_m;
        else {
            is = vee_score(MAT, hmm, query, INS, nc, rc, mt, &isp);
            ds = vee_score(MAT, hmm, query, DEL, nc, rc, mt, &dsp);

            if (is <= ds)
                return scored_path_add(&isp, INS, is, result);
            else
                return scored_path_add(&dsp, DEL, ds, result);
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
                return scored_path_add(&msp, MAT, ms, result);
            else if (is <= ds)
                return scored_path_add(&isp, INS, is, result);
            else
                return scored_path_add(&dsp, DEL, ds, result);
            break;
        case INS:
            ms = vee_score(INS, hmm, query, MAT, nc, rc, mt, &msp);
            is = vee_score(INS, hmm, query, INS, nc, rc, mt, &isp);

            if (ms <= is)
                return scored_path_add(&msp, MAT, ms, result);
            else
                return scored_path_add(&isp, INS, is, result);
            break;
        case DEL:
            ms = vee_score(DEL, hmm, query, MAT, nc, rc, mt, &msp);
            ds = vee_score(DEL, hmm, query, DEL, nc, rc, mt, &dsp);

            if (ms <= ds)
                return scored_path_add(&msp, MAT, ms, result);
            else
                return scored_path_add(&dsp, DEL, ds, result);
            break;
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
    Score *s;

    s = &mt->scores[stateRight][nc][rc];
    if (*s == NOMEMO)
        *s = vee(hmm, query, stateRight, nc, rc, mt, result);
    return *s;
}

static Score
vee_score(enum StateLabel stateRight,
          struct HMM *hmm, QuerySequence query,
          enum StateLabel state, int32_t nc, int32_t rc,
          struct MemoTable *mt, struct ScoredPath *result)
{
    Score s;

    if (stateRight != INS)
        nc--;
    if (state != DEL)
        rc--; 
    if (nc < 0 || rc < 0)
        return NEGLOGZERO; /* i.e., `internal []` */

    s = 0.0;
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
        s += hmm->nodes[nc].m_emission[RESIND(rc)];
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
        s += hmm->nodes[nc].i_emission[RESIND(rc)];
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

    return s + vee_memo(hmm, query, state, nc, rc, mt, result);
}

static struct MemoTable *
memo_table_new(int32_t hmmlen, int32_t querylen)
{
    struct MemoTable *mt;
    Score ***mts;
    int32_t i, j, k;

    mts = malloc(sizeof(*mts) * NUMLABELS);
    assert(mts != NULL);

    for (i = 0; i < NUMLABELS; i++) {
        mts[i] = malloc(sizeof(*mts[i]) * hmmlen);
        assert(mts[i] != NULL);
        for (j = 0; j < hmmlen; j++) {
            mts[i][j] = malloc(sizeof(*mts[i][j]) * querylen);
            assert(mts[i][j] != NULL);

            for (k = 0; k < querylen; k++)
                mts[i][j][k] = NOMEMO;
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
    int32_t i, j;

    for (i = 0; i < NUMLABELS; i++) {
        for (j = 0; j < mt->hmmlen; j++)
            free(mt->scores[i][j]);
        free(mt->scores[i]);
    }
    free(mt->scores);
    free(mt);
}

int
main()
{
    struct ScoredPath *result;

    result = viterbi(input_hmm, input_query);

    hmm_print(input_hmm);
    printf("\n");
    scored_path_print(result);

    free(result);
    return 0;
}
