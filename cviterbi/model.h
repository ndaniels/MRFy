#ifndef __MRFY_MODEL_H__
#define __MRFY_MODEL_H__

#include <stdint.h>

#define NUMAA 20

typedef char *QuerySequence;

typedef double Score;

typedef Score EScores[NUMAA];

Score
score_add(Score s1, Score s2);

struct Node {
    Score m_i;
    Score m_m;
    Score m_d;
    Score i_i;
    Score i_m;
    Score d_m;
    Score d_d;
    EScores m_emission;
    EScores i_emission;
};

struct HMM {
    int32_t size;
    struct Node nodes[8096];
};

void
hmm_print(struct HMM *hmm);

#endif
