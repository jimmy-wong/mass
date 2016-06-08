#!/bin/bash
export PATH

#<<EOF

echo 'input the nuclei this program is simulating:'
read nuclei

echo 'the program is running on' 
date=`date +%Y-%m-%d`
echo $date

echo 'the purpose for this running (in 140 characters):'
read purpose

echo 'the order this program is running today:'
read order

if [[ -f *.mod || -f *.o || -f distribution ]];then
    rm *.mod && rm *.o && rm distribution
fi

if [ -f *~ ]; then
    rm *~
fi

if [ -f fort* ]; then
    rm fort*
fi

#EOF

gfortran -c -g parameters.f90 
gfortran -c -g quadrupole.f90
gfortran -c -g command_args.f90
gfortran -c -g initial.f90
gfortran -c -g read_data.f90 
gfortran -c -g random_walk.f90 
gfortran -c -g mass_distribution.f90 
gfortran *.o -o distribution
./distribution "$nuclei" 6.54 "$date" "$purpose" "$order" 
#nohup ./distribution "$nuclei" 6.54 "$date" "$purpose" "$order" &
