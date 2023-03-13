# project: FLP 2022/2023 – funkcionální projekt: Haskell [Knapsack problem]
# autor: Bc. Jakub Komárek (xkomar33)
# description: program řeší optimalizační verzi 0-1 problému batohu (kna-psack problem)


.PHONY: all build doxygen run pack clean

all: build

build:
	if [ -d "build" ]; then rm -r build; fi && \
	cp -r src build && \
	cd build && \
	ghc Main.hs Minimize.hs ParseInput.hs Types.hs -Wall -o flp22-fun && \
	mv flp22-fun ../

pack: clean
	zip -r flp-fun-xkomar33.zip src/ doc/ test/  Makefile 

run:
	test -f flp22-fun && ./flp22-fun

clean:
	rm -rf flp22-fun build/ 