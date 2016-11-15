all: sharedlib bindings

sharedlib:
	g++ -shared -fPIC -ansi -pedantic -Wall cv.c cv.h -lopencv_core -lopencv_highgui -o libcv.so
	chmod 755 libcv.so
	sudo rm -f /usr/lib/libcv.so
	sudo cp libcv.so /usr/lib
	sudo ldconfig

bindings:
	ocamlfind ocamlopt -linkpkg -package ctypes,ctypes.foreign cv.ml -cclib -lcv -o cv

clean:
	rm -f cv.cmo cv.cmi cv.cmx libcv.so cv.o cv
