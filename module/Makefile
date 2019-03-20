cc = ocamlc 
Target = run 

all : $(Target) 
$(Target): amodule.cmo bmodule.cmo cmodule.cmo  
		$(cc) -o $@ $^ 
amodule.cmo : amodule.ml amodule.mli
		$(cc) -c amodule.mli
		$(cc) -c amodule.ml
bmodule.cmo : bmodule.ml 
		$(cc) -c bmodule.ml
cmodule.cmo : cmodule.ml
		$(cc) -c cmodule.ml
clean:
		rm -f *.cmx *.cmi *.o *.cmo run

