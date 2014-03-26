
DEPS=Filt.lhs DSL.hs SimplifyExp.hs TopLevel.hs Utils.hs
FLAGS=-XTemplateHaskell -XTypeSynonymInstances -XFlexibleInstances

all: test opencl
	./test
	./opencl

proglet: proglet.hs $(DEPS)
	ghc --make $(FLAGS) $< -o proglet
	./proglet 

test: Test.hs $(DEPS)
	ghc --make $(FLAGS) $< -o test 


opencl: openclBackEnd.hs $(DEPS)
	ghc --make  $(FLAGS) $< -o opencl 

clean: 
	rm -f *.o *.hi 
	rm -rf dist o0
	rm proglet test opencl