SRC = src/funks.ml
OUT = funks

OCAML = ocamlfind ocamlopt
FLAGS = -linkpkg

CMI_FILES = $(SRC:.ml=.cmi)
CMX_FILES = $(SRC:.ml=.cmx)
O_FILES = $(SRC:.ml=.o)

all: $(OUT)

$(OUT): $(CMX_FILES)
	$(OCAML) $(FLAGS) -o $(OUT) $(CMX_FILES)

$(CMX_FILES): $(SRC)
	$(OCAML) $(FLAGS) -c $<

clean:
	rm -f $(OUT) $(CMI_FILES) $(CMX_FILES) $(O_FILES) the.db

.PHONY: all clean

