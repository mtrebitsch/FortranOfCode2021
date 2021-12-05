FC=ifort
NAME=vents

FFLAGS=-cpp -O3 -xAVX -fpp -g -traceback

%.o:%.f90
	$(FC) $(FFLAGS) -c $^ -o $@

all: $(NAME) $(NAME)2

$(NAME): $(NAME).o
	$(FC) $^ -o $@

$(NAME)2: $(NAME)2.o
	$(FC) $^ -o $@

clean:
	rm -f *.mod *.o
