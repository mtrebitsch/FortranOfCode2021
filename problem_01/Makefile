FC=ifort

FFLAGS=-cpp -O3 -xAVX -fpp
SRC=sonar_sweep.f90


OBJ=${SRC:.f90=.o} #substitute .f90 with .o

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

all: sonar_sweep

sonar_sweep: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ) 

clean: #cleans all the old compilation files
	@rm -f *.mod *.o sonar_sweep
