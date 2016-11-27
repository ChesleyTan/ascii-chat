TARGET=main package
OCAMLPKGS=ctypes,ctypes.foreign,unix

all: sharedlib bindings $(TARGET)

sharedlib:
	g++ -shared -fPIC -std=c++11 -pedantic -Wall cv.c cv.h -lopencv_core -lopencv_highgui -lopencv_imgproc -lopencv_videoio -o libcv.so
	chmod 755 libcv.so
	sudo rm -f /usr/lib/libcv.so
	sudo cp libcv.so /usr/lib
	sudo ldconfig

%.cmi: %.mli
	ocamlfind ocamlopt -linkpkg -package $(OCAMLPKGS) $< -c

%.cmx: %.ml %.cmi
	ocamlfind ocamlopt -linkpkg -package $(OCAMLPKGS) $< -c

bindings: sharedlib cv.cmi cv.cmx

main: bindings main.ml
	ocamlfind ocamlopt -linkpkg -package $(OCAMLPKGS) cv.cmx main.ml -cclib -lcv -o main

package: package.cmi package.cmx

clean:
	rm -f *.cmi *.cmx *.o libcv.so main a.out
