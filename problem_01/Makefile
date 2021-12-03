FC=ifort

FFLAGS=-cpp -O3 -xAVX -fpp -g
SRC=sonar_sweep.f90 sonar_sweep2.f90


OBJ=${SRC:.f90=.o} #substitute .f90 with .o

%.o: %.f90
	$(FC) $(FFLAGS) -o $@ -c $<

all: clean sonar_sweep sonar_sweep2

sonar_sweep: $(OBJ)
	$(FC) $(FFLAGS) -o $@ sonar_sweep.o

sonar_sweep2: $(OBJ)
	$(FC) $(FFLAGS) -o $@ sonar_sweep2.o

clean: #cleans all the old compilation files
	@rm -f *.mod *.o sonar_sweep sonar_sweep2
