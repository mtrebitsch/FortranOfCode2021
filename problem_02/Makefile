FC=ifort

FFLAGS=-cpp -O3 -xAVX -fpp -g
SRC=dive.f90 dive2.f90


OBJ=${SRC:.f90=.o} #substitute .f90 with .o

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

all: dive

dive: $(OBJ)
	$(FC) $(FFLAGS) -o $@ dive.o

dive2: $(OBJ)
	$(FC) $(FFLAGS) -o $@ dive2.o 

clean: #cleans all the old compilation files
	@rm -f *.mod *.o dive dive2
