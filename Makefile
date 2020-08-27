FC = gfortran
FFLAGS = -Wall -Wpedantic -O2 -ftree-parallelize-loops=4 -march=native
SRC = fppm.f90
LIB = fppm.o
EXSRC = tests/test.f90
EXAMPLE = tests/test

all: $(LIB) $(EXAMPLE)

example: $(EXAMPLE)

lib: $(LIB)

$(LIB): $(SRC)
	$(FC) -c $(SRC) -o $(LIB) $(FFLAGS)

$(EXAMPLE): $(EXSRC) $(LIB)
	$(FC) -c $(EXSRC) -o $(EXAMPLE).o $(FFLAGS)
	$(FC) -o $(EXAMPLE) $(LIB) $(EXAMPLE).o $(FFLAGS)
