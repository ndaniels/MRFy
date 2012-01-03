all:V:
	mkdir -p .crud
	ghc -hidir .crud -odir .crud \
			--make Beta.hs Constants.hs HmmPlus Main.hs Viterbi.hs \
						 StochasticSearch.hs \
			-O3 \
			-o smurf2

optimize:V:
	mkdir -p .crud
	ghc -hidir .crud -odir .crud \
			--make Beta.hs Constants.hs HmmPlus Main.hs Viterbi.hs \
						 StochasticSearch.hs \
			-O3 \
			-fllvm \
			-o smurf2

unoptimize:V:
	mkdir -p .crud
	ghc -hidir .crud -odir .crud \
			--make Beta.hs Constants.hs HmmPlus Main.hs Viterbi.hs \
						 StochasticSearch.hs \
			-o smurf2

tags:V:
	hasktags *.hs

clean:V: 
	rm -rf .crud
	rm smurf2

