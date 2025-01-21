GF     = gfortran
# FFLAGS = -c 
FFLAGS = -c -O3 -funroll-loops -march=native -Wall
LFLAGS = -o

OBJETOS_3 = main.f95 burgers.f95 GRAVACAO_S2D.f90 est_burgers.f95\

PROG_3: $(OBJETOS_3)
	$(GF) $(LFLAGS) burgers $(OBJETOS_3)
