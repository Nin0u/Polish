BUILD = dune build

binary:
	${BUILD} polish.exe

byte:
	${BUILD} polish.bc

clean:
	dune clean