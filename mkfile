STRATS=`echo SearchStrategies/*.hs`
SRC=$STRATS Beta.hs Constants.hs FileOps.hs HMMPlus.hs Main.hs Viterbi.hs \
         StochasticSearch.hs SearchStrategy.hs \
		 HMMArby.hs HMMProps.hs \
         Wrappers.hs ConstantsGen.hs CommandArgs.hs LazySearchModel.hs \
				 Score.hs 

TGT=mrfy
CRUDOPTS= -hidir .crud -odir .crud

all:V: $TGT
optimize:V: $TGT-llvm
$TGT: $SRC
	ghc `./ghc-opts $target` --make Main.hs -o $TGT

$TGT-llvm: $SRC
	ghc `./ghc-opts $target` --make Main.hs -o $TGT

unopt:V: unoptimize
unoptimize:V: $TGT-unopt
$TGT-unopt: $SRC
	ghc `./ghc-opts $target` --make Main.hs -o $TGT-unopt
            
profile:V: ${TGT}prof

${TGT}prof: $TGT
	ghc `./ghc-opts profile2` --make Main.hs -o ${TGT}prof

tags:V:
	hasktags *.hs */*.hs

clean:V:
	rm -rf .crud-*
	rm -f $TGT ${TGT}prof
    rm -f *.hi */*.hi *.o */*.o

test:V: $TGT
	./$TGT testing/8.hmm+ testing/8.fasta

fast-test:V: $TGT
	./$prereq -test mini-strings

# note: to build profile: ghc --make Main.hs -O3 -rtsopts -o $TGT
# then, ghc --make Main.hs -O3 -prof -auto-all -caf-all -rtsopts -osuf p_o -o $TGT
