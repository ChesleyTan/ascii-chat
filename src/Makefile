TARGET=main
OBJ=package.cmx main.cmx
OCAMLPKGS=ctypes,ctypes.foreign,unix,yojson,lz4

all: clean sharedlib bindings $(TARGET)

sharedlib: cv.cpp cv.h
	g++ -shared -fPIC -std=c++11 -pedantic -Wall cv.cpp -lopencv_core -lopencv_highgui -lopencv_imgproc -lopencv_videoio -o libcv.so
	chmod 755 libcv.so
	sudo rm -f /usr/local/lib/libcv.so
	sudo cp libcv.so /usr/local/lib
	sudo ldconfig || echo "Warning: ldconfig not found"

%.cmi: %.mli
	ocamlfind ocamlopt -linkpkg -package $(OCAMLPKGS) $< -c

%.cmx: %.ml %.cmi
	ocamlfind ocamlopt -linkpkg -package $(OCAMLPKGS) $< -c

bindings: sharedlib cv.cmi cv.cmx
	@echo Building CV bindings

main: bindings $(OBJ)
	ocamlfind ocamlopt -linkpkg -package $(OCAMLPKGS) cv.cmx $(OBJ) -cclib -lcv -o ascii-chat

clean:
	rm -f *.cmi *.cmx *.o libcv.so
