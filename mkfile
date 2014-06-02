STRATS=`echo SearchStrategies/*.hs`
SRC=$STRATS Beta.hs Constants.hs FileOps.hs HMMPlus.hs Main.hs Viterbi.hs \
         StochasticSearch.hs SearchStrategy.hs \
		 HMMArby.hs HMMProps.hs \
         Wrappers.hs ConstantsGen.hs CommandArgs.hs LazySearchModel.hs \
				 Score.hs Perturb.hs

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

test-sandwich:V: $TGT
	./$TGT testing/sandwich.hmm+ testing/sandwich.fasta

build:V:
	cabal configure
	cabal build
	cp dist/build/mrfy/mrfy ./

bench-short:V:
	(cabal install && in-dir mrfy-bench ./mrfy-bench -s)

bench-short-old:V:
	(cabal install && in-dir mrfy-bench ./mrfy-bench -s -old)

dot:V:
	dot -Tpdf -o tree.pdf tree.dot
	dot -Tpdf -o model.pdf model.dot

cinput-micro:V: build
	dist/build/mrfy/mrfy -dumpc testing/micro8.hmm+ testing/micro8.fasta \
		> cviterbi/input_hmm.c

cinput:V: build
	dist/build/mrfy/mrfy -dumpc testing/8.hmm+ testing/8.fasta \
		> cviterbi/input_hmm.c

mlinput-micro:V: build
	dist/build/mrfy/mrfy -dumpml testing/micro8.hmm+ testing/micro8.fasta \
		> mlviterbi/input_hmm.sml

mlinput:V: build
	dist/build/mrfy/mrfy -dumpml testing/8.hmm+ testing/8.fasta \
		> mlviterbi/input_hmm.sml

# note: to build profile: ghc --make Main.hs -O3 -rtsopts -o $TGT
# then, ghc --make Main.hs -O3 -prof -auto-all -caf-all -rtsopts -osuf p_o -o $TGT
