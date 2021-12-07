BUILD = dune build

binary:
	${BUILD} polish.exe

byte:
	${BUILD} polish.bc

clean:
	dune clean

buildTests: buildReadPolish buildPrintPolish buildEvalPolish

buildReadPolish:
	${BUILD} tests/readPolish/testReadPolish.exe

buildPrintPolish:
	${BUILD} tests/printPolish/testPrintPolish.exe

buildEvalPolish:
	${BUILD} tests/evalPolish/testEvalPolish.exe