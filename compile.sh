#!/bin/bash
export PATH
rm nohup.out
rm mass
rm Metropolis*.o
gfortran -c -g Mod_Command_args.f90
gfortran -c -g Mod_Preparation.f90
gfortran -c -g Mod_Grid.f90
gfortran -c -g Mod_Shape.f90
gfortran -c -g GetValue.f90
gfortran -c -g Random_walk.f90
gfortran -c -g Metropolis.f90
#gfortran -c -g Metropolis_tmp.f90
gfortran -c -g Cal_Vbias.f90
gfortran -c -g Read_the_data.f90
gfortran -c -g Mass_Distribution.f90
gfortran -g *.o -o mass
