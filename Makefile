# NOTE: Requires all sourcecode in a single directory
# TODO: module dependencies are not expressed
EXEC = datatm

# Override CURDIR to reduce some of the clutter
# Remove this line if you want to use explicit paths
CURDIR = .

SRC = $(CURDIR)/src
OBJ = $(CURDIR)/obj
INC = $(CURDIR)/inc
LIB = $(CURDIR)/lib
BIN = $(CURDIR)/bin

#OASIS_BASE = /short/v45/mxw157/MCT
OASIS_BASE = /short/v45/mxw157/projects/xp_access

FC_OASIS = -I$(OASIS_BASE)/build/lib/psmile.MPI1

LD_OASIS = -L$(OASIS_BASE)/lib -lpsmile.MPI1 -lmct -lmpeu -lscrip
LD_MPI = -L$(OPENMPI_BASE)/lib -lmpi_f90 -lmpi_f77 -lmpi
LD_NETCDF = -L$(NETCDF_BASE)/lib -lnetcdff -lnetcdf

FC = ifort
FC_FLAGS = $(FC_OASIS) -module $(INC) -I$(INC) -g -ftrapuv
LD_FLAGS = $(LD_NETCDF) $(LD_MPI) $(LD_OASIS) -limf -lm

SRCS = $(shell find $(SRC) -type f -name '*.f90')
OBJS = $(patsubst $(SRC)%, $(OBJ)%, $(SRCS:.f90=.o))

all: $(OBJ) $(INC) $(BIN) $(BIN)/$(EXEC)

$(BIN)/$(EXEC): $(OBJS)
	$(FC) -o $@ $^ $(LD_FLAGS)

$(OBJ)/%.o: $(SRC)/%.f90
	$(FC) -c -o $@ $< $(FC_FLAGS)

$(OBJ)/datatm.o: $(SRC)/datatm.f90 $(OBJ)/coupler.o $(OBJ)/mpp.o \
				 $(OBJ)/field.o $(OBJ)/str.o
 
$(OBJ)/coupler.o: $(SRC)/coupler.f90 $(OBJ)/mpp.o

$(OBJ)/field.o: $(SRC)/field.f90 $(OBJ)/str.o

$(OBJ) $(INC) $(BIN):
	mkdir -p $@

clean:
	rm -f $(BIN)/$(EXEC) $(OBJS) $(INC)/*.mod

.PHONY: all clean
