# Makefile for YFrob AFP demos

.PHONY: all configure build install clean

TARGET = afp-demos

all: build

configure:
	runhaskell Setup.hs configure --ghc

build: configure
	runhaskell Setup.hs build
	-rm ${TARGET}
	ln -s dist/build/${TARGET}/${TARGET} ${TARGET}

install: build
	runhaskell Setup.hs install

clean:
	runhaskell Setup.hs clean
	-rm ${TARGET}
