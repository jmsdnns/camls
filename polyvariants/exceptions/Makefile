EXEC = pve
OCAMLC = ocamlfind ocamlc
SRC = src/pve.ml
IFACE = src/pve.mli
SRC_OBJECTS = src/pve.cmo
IFACE_OBJECTS = src/pve.cmi

all: $(EXEC)

$(EXEC): $(SRC_OBJECTS) 
	$(OCAMLC) -o $(EXEC) $(SRC_OBJECTS)

$(SRC_OBJECTS): $(SRC) $(IFACE_OBJECTS)
	$(OCAMLC) -c $(SRC) -cmi-file $(IFACE_OBJECTS)

$(IFACE_OBJECTS): $(IFACE)
	$(OCAMLC) $<

clean:
	rm -f $(EXEC) $(SRC_OBJECTS) $(IFACE_OBJECTS)

.PHONY: all clean
