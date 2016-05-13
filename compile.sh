#!/bin/bash
export PATH

rm *.mod
rm *.o
gfortran -c parameters.f90 
gfortran -c quadrupole.f90
gfortran -c command_args.f90
gfortran -c initial.f90
gfortran -c read_data.f90 
gfortran -c random_walk.f90 
gfortran -c mass_distribution.f90 
gfortran *.o 
