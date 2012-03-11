STRATS=`echo SearchStrategies/*.hs`
SRC=$STRATS Beta.hs Constants.hs HmmPlus.hs Main.hs Viterbi.hs \
         StochasticSearch.hs SearchStrategy.hs \
         Wrappers.hs ConstantsGen.hs

TGT=smurf2
CRUDOPTS= -hidir .crud -odir .crud
OPTS= -fspec-constr-count=6

all:V:
	mkdir -p .crud
	ghc $OPTS $CRUDOPTS --make $SRC \
			-O3 \
			-threaded \
			-rtsopts \
			-o $TGT

optimize:V:
	mkdir -p .crud
	ghc $OPTS $CRUDOPTS --make $SRC \
			-O3 \
			-fllvm \
			-o $TGT

unopt:V: unoptimize
unoptimize:V:
	mkdir -p .crud
	ghc $OPTS $CRUDOPTS --make $SRC -o $TGT
            
profile:V:
    mkdir -p .crud
    ghc $OPTS $CRUDOPTS \
        --make Main.hs -O3 -fforce-recomp \
         -rtsopts \
         -o $TGT
    ghc $OPTS $CRUDOPTS \
        --make Main.hs -O3 -prof -fforce-recomp \
         -auto-all -caf-all -rtsopts -osuf p_o \
         -o ${TGT}prof

tags:V:
	hasktags *.hs

clean:V: 
	rm -rf .crud
	rm -f $TGT

# note: to build profile: ghc --make Main.hs -O3 -rtsopts -o $TGT
# then, ghc --make Main.hs -O3 -prof -auto-all -caf-all -rtsopts -osuf p_o -o $TGT
