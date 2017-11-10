
DEPS=filtFIR.lhs expSimplifyDSL.hs expSimplify.hs filtExamples.hs expSimplifyUtils.hs
FLAGS=-XTemplateHaskell -XTypeSynonymInstances -XFlexibleInstances

all: test opencl
	./test
	./opencl

test: expTest.hs $(DEPS)
	ghc --make $(FLAGS) $< -o test 

opencl: codeGenOpenCL.hs $(DEPS)
	ghc --make  $(FLAGS) $< -o opencl 

clean: 
	rm -f *.o *.hi 
	rm -rf dist o0
	rm test opencl