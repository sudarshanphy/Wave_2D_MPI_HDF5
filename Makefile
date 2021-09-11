##############################################################
#
# Makefile for 2D Wave equation with a source term.
# sneopane, June 2021
#
###############################################################
# Makefile format is always:
#
# target ... : prerequisites ...
# 	receipe
#
# Target:
# What to generate if it doesn’t exist or is older than
# prerequisites (e.g. object files, executables)
#
# Prerequisites:
# Items which need to be generated before executing the recipe
#
# Recipe:
# Instructions that make executes (must have a TAB character
# before)
# ##############################################################

# Path to src folder

SRC_FOLD=src

# Fortran compiler to use

FC=mpif90

# Flags for the compiler
#FFLAGS=-O2 -fimplicit-none  -Wall  -Wline-truncation  -Wcharacter-truncation  -Wsurprising  -Waliasing  -Wimplicit-interface  -Wunused-parameter  -fwhole-file  -fcheck=all  -std=f2008  -pedantic  -fbacktrace
FFLAGS=-O3 -Wall -Wextra -fbacktrace

# Path to HDF5 library and include folder: these are the default path
# when ' sudo apt-get install libhdf5-dev ' is used to install hdf5
# path is found using ' whereis hdf5 '
# library path

HDF5_LIB=usr/lib/x86_64-linux-gnu/hdf5/serial

# include path

HDF5_INC=usr/include/hdf5/serial

# Files that we need to compile to produce the executable

SRC=variables.f90 write_hdf5_file.f90 get_param.f90 initialize.f90 \
    source.f90 fdm_solver.f90 boundary_condition.f90 communication.f90 \
    main.f90

# OBJ has stored is like SRC but with all files having *.o

OBJ=${SRC:.f90=.o}

# Loop (%) over *.f90 files to create *.o files
# $@ represents the target files and $< represents the prerequisite files

%.o: $(SRC_FOLD)/%.f90
	$(FC) $(FFLAGS) -o $(SRC_FOLD)/$@ -I/$(HDF5_INC) -c $<   

# Generate the executable (wave_2d) provided all the *.o files are present
# After that it creates a symbolic link for the parameter input file in
# present directory.

EXEC=wave_2d_mpi

# Since all the *.o files are inside /src directory
NEW_OBJ=${OBJ:%.o=$(SRC_FOLD)/%.o}

$(EXEC): $(OBJ)
	$(FC) $(FFLAGS) -L/$(HDF5_LIB) -o $@ $(NEW_OBJ) -lhdf5 -lhdf5_fortran
	@ln -s  $(SRC_FOLD)/par_input.par ./par_input.par
	@mkdir ./output_files 

# Remove *.mod, *.o, and wave_2d (executable)
# To hide the command below from printing, just modify rm as @rm (just add @)
# Also, unlink previously created parameter input file.

clean:
	rm -f *.mod wave_2d_mpi
	rm -f $(SRC_FOLD)/*.o
	@unlink par_input.par

# Remove all the .h5 files (output files) from the output_files directory

clean_output:
	@rm -f ./output_files/*.h5

# Combined command to acomplish both clean and clean_output

clean_all:
	rm -f *.mod wave_2d_mpi
	rm -f $(SRC_FOLD)/*.o
	@unlink par_input.par
	@rm -f ./output_files/*.h5
	@rm -r ./output_files
	
