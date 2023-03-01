main: main.cmo
	ocamlopt -o main.cmo

main.cmo: epl.cmo main.ml
	ocamlfind ocamlc -linkpkg -package delimcc -thread  pMap.cmo epl.cmo main.ml

epl.cmo: pMap.cmo pMap.cmi epl.ml
	ocamlfind ocamlc -linkpkg -package delimcc -thread pMap.cmo epl.ml

pMap.cmo: pMap.cmi
	ocamlc pMap.ml

pMap.cmi:
	ocamlc pMap.mli

clean:
	rm *.cmo *.cmi *.out
