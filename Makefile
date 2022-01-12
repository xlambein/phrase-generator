DEST="dist"
TARGET="target"

.PHONY = build copy

build:
	mkdir -p $(TARGET)
	elm make src/Main.elm --output=$(TARGET)/main.js

dist: build
	mkdir -p $(DEST)
	cp $(TARGET)/main.js $(DEST)
	cp index.html $(DEST)
	cp style.css $(DEST)