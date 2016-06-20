#HSFLAGS=-prof -fforce-recomp -auto-all -caf-all --make -O2
HSFLAGS=-w -XFlexibleContexts
.PHONY:clean

all: compact

compact:
	ghc logicalParser 	$(HSFLAGS) 

clean:
	-@rm -v *.o *.hi 2> /dev/null || true
