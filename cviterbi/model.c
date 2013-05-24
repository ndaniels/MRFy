#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "model.h"

#define PRINT_SCORE(lbl, s) printf("(" lbl ": %0.4f) ", (s));

static void
hmm_print_node(struct Node *);

void
hmm_print(struct HMM *hmm)
{
    int32_t i;

    printf("HMM: %d nodes\n", hmm->size);
    for (i = 0; i < hmm->size; i++)
        hmm_print_node(&hmm->nodes[i]);
}

static void
hmm_print_node(struct Node *node)
{
    int32_t i;

    printf("\t");

    PRINT_SCORE("m_i", node->m_i);
    PRINT_SCORE("m_m", node->m_m);
    PRINT_SCORE("m_d", node->m_d);
    PRINT_SCORE("i_i", node->i_i);
    PRINT_SCORE("i_m", node->i_m);
    PRINT_SCORE("d_m", node->d_m);
    PRINT_SCORE("d_d", node->d_d);

    printf("\n");

    printf("\t(ME: ");
    for (i = 0; i < NUMAA; i++)
        printf("%0.4f ", node->m_emission[i]);
    printf(") ");

    printf("\n");

    printf("\t(IE: ");
    for (i = 0; i < NUMAA; i++)
        printf("%0.4f ", node->i_emission[i]);
    printf(") ");

    printf("\n");
    printf("\n");
}
