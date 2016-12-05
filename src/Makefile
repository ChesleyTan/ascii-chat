TARGET=main test
OBJ=package.cmx messaging.cmx network.cmx state.cmx view.cmx test_messaging.cmx
OCAMLPKGS=ctypes,ctypes.foreign,unix,yojson,lwt,lwt.preemptive,lwt.unix,cryptokit,oUnit
OPTS=-thread

all: clean sharedlib bindings $(TARGET)

sharedlib: libasciichat_cv.so libasciichat_utils.so
	@(which ldconfig > /dev/null && sudo ldconfig) || true

libasciichat_cv.so: cv.cpp cv.h
	@echo "Compiling and linking OpenCV bindings"
	g++ -shared -fPIC -std=c++11 -pedantic -Wall cv.cpp -lopencv_core -lopencv_highgui -lopencv_imgproc -lopencv_videoio -o libasciichat_cv.so
	chmod 755 libasciichat_cv.so
	sudo rm -f /usr/local/lib/libasciichat_cv.so
	sudo cp libasciichat_cv.so /usr/local/lib

libasciichat_utils.so: utils.c utils.h
	@echo "Compiling and linking utilities"
	gcc -shared -fPIC -Wall utils.c -o libasciichat_utils.so
	chmod 755 libasciichat_utils.so
	sudo rm -f /usr/local/lib/libasciichat_utils.so
	sudo cp libasciichat_utils.so /usr/local/lib

%.cmi: %.mli
	ocamlfind ocamlopt $(OPTS) -linkpkg -package $(OCAMLPKGS) $< -c

%.cmx: %.ml %.cmi
	ocamlfind ocamlopt $(OPTS) -linkpkg -package $(OCAMLPKGS) $< -c

bindings: cv.cmi cv.cmx utils.cmi utils.cmx

main: bindings $(OBJ) main.cmx
	ocamlfind ocamlopt $(OPTS) -linkpkg -package $(OCAMLPKGS) cv.cmx utils.cmx $(OBJ) main.cmx -cclib -lasciichat_cv -cclib -lasciichat_utils -o ascii-chat

test: bindings $(OBJ) test_main.cmx
	ocamlfind ocamlopt $(OPTS) -linkpkg -package $(OCAMLPKGS) cv.cmx utils.cmx $(OBJ) test_main.cmx -cclib -lasciichat_cv -cclib -lasciichat_utils -o test_main

clean:
	rm -f ascii-chat *.cmi *.cmx *.o

.PHONY: default all clean sharedlib utils bindings main test no-bindings
