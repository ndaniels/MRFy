#ifndef __MRFY_VITERBI_H__
#define __MRFY_VITERBI_H__

enum StateLabel { MAT=0, INS, DEL, NUMLABELS };
// INS must follow MAT
const char *label_names[] = { "Mat", "Ins", "Del", "out of bounds" };

struct ScoredPath {
    Score score;
    int32_t path_len; // number of labels in 'path'
    enum StateLabel path[];
};

struct ScoredPath *
viterbi(struct HMM *hmm, QuerySequence query);

Score
scored_path_combine(struct ScoredPath *sp, enum StateLabel state, Score score,
                    struct ScoredPath *result);

void
scored_path_print(struct ScoredPath *sp);

#endif
