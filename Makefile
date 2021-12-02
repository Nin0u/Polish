BUILD = dune build
EXEC = dune exec

binary:
	${BUILD} polish.exe

byte:
	${BUILD} polish.bc

clean:
	dune clean

buildTests: 
	${BUILD} tests/readPolish/testReadPolish.exe

testReadPolish:
	${BUILD} tests/readPolish/testReadPolish.exe
	${EXEC} tests/readPolish/testReadPolish.exe

testPrintPolish:
	${BUILD} tests/printPolish/testPrintPolish.exe
	${EXEC} tests/printPolish/testPrintPolish.exe print_polish

testEvalPolish:
	${BUILD} tests/evalPolish/testEvalPolish.exe
	${EXEC} tests/evalPolish/testEvalPolish.exe eval_polish