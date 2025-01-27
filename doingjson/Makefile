SRC = src/doingjson.ml
OUT = doingjson

OCAML = ocamlfind ocamlopt
FLAGS = -thread -linkpkg
PACKAGES = ppx_deriving,ppx_deriving.show,ppx_deriving.eq,ppx_deriving.ord,yojson,ppx_deriving_yojson

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

