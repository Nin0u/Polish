BUILD = dune build
EXEC = dune exec

binary:
	${BUILD} polish.exe

byte:
	${BUILD} polish.bc

clean:
	dune clean

testReadPolish:
	${BUILD} tests/testReadPolish.exe