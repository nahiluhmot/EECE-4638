HC=cabal

BUILD_FLAGS=
BUILD_COMMAND=build
BUILD_DIR=dist

INSTALL_FLAGS=--only-dependencies --enable-tests
INSTALL_COMMAND=install

INPUT_DIR=data
OUTPUT_DIR=out
INPUTS=$(wildcard $(INPUT_DIR)/*.input)
OUTPUTS=$(patsubst $(INPUT_DIR)/%.input, $(OUTPUT_DIR)/%.output, $(INPUTS))
EXE=dist/build/knapsack-five/knapsack-five

ZIP=hullun-2a.zip
FILES=$(shell git ls-files)

all: build

clean:
	rm -rf $(BUILD_DIR) $(OUTPUT_DIR)

install:
	$(HC) $(INSTALL_COMMAND) $(INSTALL_FLAGS)

build: $(EXE)

generate: $(OUTPUTS)

zip: $(ZIP)

$(EXE): install
	$(HC) $(BUILD_COMMAND) $(BUILD_FLAGS)

$(OUTPUT_DIR):
	mkdir -p $(OUTPUT_DIR)

$(OUTPUT_DIR)/%.output: $(INPUT_DIR)/%.input build $(OUTPUT_DIR)
	$(EXE) $< > $@

$(ZIP): $(FILES)
	zip -r $(ZIP) $(FILES)

.PHONY: all clean install build generate zip
