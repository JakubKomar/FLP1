.PHONY: all build doxygen run pack clean

all: build

build:
	if [ -d "build" ]; then rm -r build; fi && \
	cp -r src build && \
	cd build && \
	ghc Main.hs -Wall -o flp22-fun && \
	mv flp22-fun ../

pack: clean
	zip -r flp-fun-xkomar33.zip src/ doc/ test/  Makefile 

run:
	test -f flp22-fun && ./flp22-fun

clean:
	rm -rf flp22-fun build/ 