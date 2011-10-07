all:V:
	ghc --make *.hs -o smurf2

tags:V:
	hasktags *.hs

clean:V: 
	rm *.{hi,o}
	rm smurf2
