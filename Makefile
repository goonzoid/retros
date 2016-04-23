ELM_FILES = $(shell find elm/ -type f -name '*.elm')

all: public/elm.js

.PHONY: clean

clean:
	rm -f public/elm.js

public/elm.js: $(ELM_FILES)
	cd elm && elm make --warn --output ../public/elm.js src/Main.elm
