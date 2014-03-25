
DEPS=Filt.lhs DSL.hs SimplifyExp.hs TopLevel.hs Utils.hs

all: test opencl
	./test
	./opencl

proglet: proglet.hs $(DEPS)
	ghc --make -XTemplateHaskell -XTypeSynonymInstances -XFlexibleInstances $< -o proglet
	./proglet 

test: Test.hs $(DEPS)
	ghc --make -XTemplateHaskell -XTypeSynonymInstances -XFlexibleInstances $< -o test 


opencl: openclBackEnd.hs $(DEPS)
	ghc --make $< -o opencl 

clean: 
	rm -f *.o *.hi 
	rm -rf dist o0
	rm proglet test opencl