all: hello_world.native

hello_world.native: hello_world.ml
	ocamlbuild -pkg opium -pkg batteries -pkg lwt.ppx hello_world.native

clean:
	rm *.native