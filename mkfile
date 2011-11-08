all:V:
	mkdir -p .crud
	ghc -hidir .crud -odir .crud --make *.hs -o smurf2

optimize:V:
	mkdir -p .crud
	ghc -hidir .crud -odir .crud --make *.hs -O3 -o smurf2

tags:V:
	hasktags *.hs

clean:V: 
	rm -rf .crud
	rm smurf2

