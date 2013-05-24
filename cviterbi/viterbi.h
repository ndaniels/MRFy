#ifndef __MRFY_VITERBI_H__
#define __MRFY_VITERBI_H__

#define PATH_CAP 8096
#define NUMLABELS 3

enum StateLabel { MAT, INS, DEL };

struct ScoredPath {
    Score score;
    enum StateLabel path[PATH_CAP];
    int32_t path_len;
};

struct ScoredPath *
viterbi(struct HMM *hmm, QuerySequence query);

Score
scored_path_add(struct ScoredPath *sp, enum StateLabel state, Score score,
                struct ScoredPath *result);

void
scored_path_print(struct ScoredPath *sp);

#endif
