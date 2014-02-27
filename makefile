
all: proglet.hs
	ghc --make -XTemplateHaskell -XTypeSynonymInstances -XFlexibleInstances $< -o proglet
	./proglet

sim: proglet.hs
	ghc --make -XTemplateHaskell $< -o proglet
	./proglet

clean: 
	rm -f *.o *.hi 
	rm -rf dist o0
	rm proglet