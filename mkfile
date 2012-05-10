STRATS=`echo SearchStrategies/*.hs`
SRC=$STRATS Beta.hs Constants.hs FileOps.hs HmmPlus.hs Main.hs Viterbi.hs \
         StochasticSearch.hs SearchStrategy.hs \
		 HMMArby.hs HMMProps.hs \
         Wrappers.hs ConstantsGen.hs CommandArgs.hs SearchModel.hs \
				 Score.hs 

TGT=mrfy
CRUDOPTS= -hidir .crud -odir .crud

all:V:
	ghc `./ghc-opts $target` --make Main.hs -o $TGT

optimize:V:
	ghc `./ghc-opts $target` --make Main.hs -o $TGT

unopt:V: unoptimize
unoptimize:V:
	ghc `./ghc-opts $target` --make Main.hs -o $TGT
            
profile:V:
	ghc `./ghc-opts $target`  --make Main.hs -o $TGT
	ghc `./ghc-opts profile2` --make Main.hs -o ${TGT}prof

tags:V:
	hasktags *.hs

clean:V:
	rm -rf .crud-*
	rm -f $TGT ${TGT}prof

test:V: $TGT
	./$TGT testing/8.hmm+ testing/8.fasta


# note: to build profile: ghc --make Main.hs -O3 -rtsopts -o $TGT
# then, ghc --make Main.hs -O3 -prof -auto-all -caf-all -rtsopts -osuf p_o -o $TGT
