SRC = src/jmap.ml
OUT = jmap

OCAML = ocamlfind ocamlopt
FLAGS = -thread -linkpkg
PACKAGES = lwt,lwt.unix,cmdliner

CMI_FILES = $(SRC:.ml=.cmi)
CMX_FILES = $(SRC:.ml=.cmx)
O_FILES = $(SRC:.ml=.o)

all: $(OUT)

$(OUT): $(CMX_FILES)
	$(OCAML) $(FLAGS) -package $(PACKAGES) -o $(OUT) $(CMX_FILES)

$(CMX_FILES): $(SRC)
	$(OCAML) $(FLAGS) -package $(PACKAGES) -c $<

clean:
	rm -f $(OUT) $(CMI_FILES) $(CMX_FILES) $(O_FILES)

.PHONY: all clean

