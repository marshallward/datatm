# TODO: module dependencies are not expressed
# TODO: replace mkdir calls with pre-created directories in repo?
EXEC = datatm

# Override CURDIR to reduce some of the clutter
# Remove this line if you want to use explicit paths
CURDIR = .

SRC = $(CURDIR)/src
OBJ = $(CURDIR)/obj
INC = $(CURDIR)/inc
LIB = $(CURDIR)/lib
BIN = $(CURDIR)/bin

OASIS_BASE = /short/v45/mxw157/MCT

FC_OASIS = -I$(OASIS_BASE)/build/lib/psmile.MPI1

LD_OASIS = -L$(OASIS_BASE)/lib -lpsmile.MPI1 -lmct -lmpeu -lscrip
LD_MPI = -L$(OPENMPI_BASE)/lib -lmpi_f90 -lmpi_f77 -lmpi
LD_NETCDF = -L$(NETCDF_BASE)/lib -lnetcdff -lnetcdf

FC = ifort
FC_FLAGS = $(FC_OASIS) -module $(INC) -I$(INC)
LD_FLAGS = $(LD_NETCDF) $(LD_MPI) $(LD_OASIS) -limf -lm

SRCS = $(shell find $(SRC) -type f -name '*.f90')
OBJS = $(patsubst $(SRC)%, $(OBJ)%, $(SRCS:.f90=.o))

$(BIN)/$(EXEC): $(OBJS)
	mkdir -p $(BIN)
	$(FC) -o $@ $^ $(LD_FLAGS)

$(OBJ)/datatm.o: $(SRC)/datatm.f90 $(OBJ)/coupler.o
	mkdir -p $(INC)
	mkdir -p $(OBJ)
	$(FC) -c -o $@ $< $(FC_FLAGS)

$(OBJ)/%.o: $(SRC)/%.f90
	mkdir -p $(INC)
	mkdir -p $(OBJ)
	$(FC) -c -o $@ $< $(FC_FLAGS)

clean:
	rm -f $(BINS) $(OBJS) $(INC)/*.mod

.PHONY: clean
