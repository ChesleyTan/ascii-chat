all:
	g++ -shared -fPIC -ansi -pedantic -Wall foo.c foo.h -o libfoo.so
	chmod 755 libfoo.so
	sudo cp libfoo.so /usr/bin
	sudo ldconfig
	ocamlfind ocamlc -linkpkg -custom -package ctypes,ctypes.foreign foo.ml -cclib -lfoo -o foo
clean:
	rm -f foo.cmo foo.cmi foo.cmx libfoo.so foo
