# Makefile for YFrob RobotSim test

.PHONY: all configure build install clean

TARGET = robotsim-test

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
