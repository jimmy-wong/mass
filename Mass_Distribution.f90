program Mass_Distribution
use command_args,only:init_command_args,command1
  use Mod_Preparation, only: n_lrzcs,Q0,output
  use Mod_Grid, only: Init_Mod_Grid
  implicit none

  integer::length, status
  character(len=10)::command
  real :: atomvalue
  integer :: i_random_walk,i_counts
  integer,dimension(5,2)::ind

  call Init_command_args
  i_counts=0
  open(50,file='/home/zmwang/Documents/mass_result/'//trim(command1)//'.txt',status='replace',action='write')

  call Read_the_data()

  call random_seed()
  do i_random_walk = 1,10000
     call Random_walk(atomvalue)
     write(50,'(4x,f10.5)') atomvalue
     i_counts=i_counts+1
     write(*,*) i_counts
     write(*,'(4x,f10.5)') atomvalue
  enddo

  close(50)

end program Mass_Distribution
