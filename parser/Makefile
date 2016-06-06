#HSFLAGS=-prof -fforce-recomp -auto-all -caf-all --make -O2
HSFLAGS=-w
.PHONY:clean

all: compact expand

compact:
	ghc compact 	$(HSFLAGS) 

expand:
	ghc expand      $(HSFLAGS)

clean:
	-@rm -v *.o *.hi 2> /dev/null || true
