SRC = src/thecommas.ml
OUT = thecommas

OCAML = ocamlfind ocamlopt
FLAGS = -linkpkg
PACKAGES = csv,yojson

CMI_FILES = $(SRC:.ml=.cmi)
CMX_FILES = $(SRC:.ml=.cmx)
O_FILES = $(SRC:.ml=.o)

all: $(OUT)

$(OUT): $(CMX_FILES)
	$(OCAML) $(FLAGS) -package $(PACKAGES) -o $(OUT) $(CMX_FILES)

$(CMX_FILES): $(SRC)
	$(OCAML) $(FLAGS) -package $(PACKAGES) -c $<

clean:
	rm -f $(OUT) $(CMI_FILES) $(CMX_FILES) $(O_FILES) the.db

.PHONY: all clean

