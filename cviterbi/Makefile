CC=gcc
CFLAGS=-g -O3 -std=c99 -Werror -Wall -Wextra -pedantic
OBJS=viterbi.o model.o input_query.o input_hmm.o

viterbi: $(OBJS)

viterbi.o: model.h viterbi.h
model.o: model.h

clean:
	rm -f viterbi
	rm -f *.o

