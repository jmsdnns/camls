SRC = src/downloader.ml
OUT = dl

OCAML = ocamlfind ocamlopt
FLAGS = -thread -linkpkg
PACKAGES = lwt,cohttp-lwt-unix,lwt_ssl

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

