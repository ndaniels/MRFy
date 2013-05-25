#ifndef __MRFY_VITERBI_H__
#define __MRFY_VITERBI_H__

#include "model.h"

typedef enum { Mat=0, Ins, Del, NUMLABELS } StateLabel;
// INS must follow MAT
const char *label_names[] = { "Mat", "Ins", "Del", "out of bounds" };

Score
viterbi_memo(struct HMM *hmm, QuerySequence query);

Score
viterbi_dp(struct HMM *hmm, QuerySequence query);

#endif
