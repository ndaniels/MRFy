all:V:
	mkdir -p .crud
	ghc -hidir .crud -odir .crud \
			--make Beta.hs Constants.hs HmmPlus Main.hs Viterbi.hs \
						 StochasticSearch.hs SearchStrategy.hs \
						 SearchStrategies/*.hs \
						 Wrappers.hs \
						 ConstantsGen.hs \
			-O3 \
			-threaded \
			-rtsopts \
			-o smurf2

optimize:V:
	mkdir -p .crud
	ghc -hidir .crud -odir .crud \
			--make Beta.hs Constants.hs HmmPlus Main.hs Viterbi.hs \
						 StochasticSearch.hs SearchStrategy.hs \
						 SearchStrategies/*.hs \
						 Wrappers.hs \
						 ConstantsGen.hs \
			-O3 \
			-fllvm \
			-o smurf2

unoptimize:V:
	mkdir -p .crud
	ghc -hidir .crud -odir .crud \
			--make Beta.hs Constants.hs HmmPlus Main.hs Viterbi.hs \
						 StochasticSearch.hs SearchStrategy.hs \
						 SearchStrategies/*.hs \
						 Wrappers.hs \
						 ConstantsGen.hs \
			-o smurf2
            
profile:V:
    mkdir -p .crud
    ghc -hidir .crud -odir .crud \
        --make Main.hs -O3 -fforce-recomp \
         -rtsopts \
         -o smurf2
    ghc -hidir .crud -odir .crud \
        --make Main.hs -O3 -prof -fforce-recomp \
         -auto-all -caf-all -rtsopts -osuf p_o \
         -o smurf2prof

tags:V:
	hasktags *.hs

clean:V: 
	rm -rf .crud
	rm smurf2

# note: to build profile: ghc --make Main.hs -O3 -rtsopts -o smurf2
# then, ghc --make Main.hs -O3 -prof -auto-all -caf-all -rtsopts -osuf p_o -o smurf2
