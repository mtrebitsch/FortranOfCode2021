FC=ifort

FFLAGS=-cpp -O3 -xAVX -fpp -g -traceback
SRC=diagnostic.f90 diagnostic2.f90


OBJ=${SRC:.f90=.o} #substitute .f90 with .o

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

all: diagnostic diagnostic2

diagnostic: $(OBJ)
	$(FC) $(FFLAGS) -o $@ diagnostic.o

diagnostic2: $(OBJ)
	$(FC) $(FFLAGS) -o $@ diagnostic2.o 

clean: #cleans all the old compilation files
	@rm -f *.mod *.o diagnostic diagnostic2
