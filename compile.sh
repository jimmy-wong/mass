#!/bin/bash
export PATH

rm *.mod
rm *.o
gfortran -c -g parameters.f90 
gfortran -c -g quadrupole.f90
gfortran -c -g command_args.f90
gfortran -c -g initial.f90
gfortran -c -g read_data.f90 
gfortran -c -g random_walk.f90 
gfortran -c -g mass_distribution.f90 
gfortran *.o -fopenmp -o mass
./mass U236 6.54
