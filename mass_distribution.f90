program Mass_Distribution
  use command_args,only:init_command_args,command1,command3,command4,command5
  use parameters, only: gs,nuc,n_lrzcs,Q0,output,E_excited

  implicit none
  external :: read_the_data

  real :: atomvalue
  integer :: i_random_walk,i_counts
  integer,dimension(5,2)::ind

  call init_command_args
  i_counts=0
  open(50,file='/home/zmwang/mass/'//trim(command1)//'_'//trim(command3)//'_'//trim(command5)//'.txt'&
       ,action='write',asynchronous='yes')
  call read_the_data

!  call random_seed()

  !-----------------declarition of each run------------------------  
  write(50,*) '-----',trim(command3),'-----'
  write(50,*) '-----declaration-----'
  write(50,'(4a,f6.3,4a)') 'parameters:',char(10),char(9),'excitation energy:',E_excited,char(10),char(9),'nuclei:',trim(command1)
  write(50,'(a)') 'this file contains the atomic number of the heavy fission fragments'
  write(50,'(a)') 'we run the program 10,000 times to simulate mass distribution'
  write(50,'(a,a)') 'purpose:',trim(command4)
  !----------------------------------------------------------------

  do i_random_walk = 1,10000
     call Random_walk(atomvalue)
     write(50,'(4x,f8.2)') atomvalue
     write(*,'(4x,f8.2)') atomvalue
  enddo
  close(50)
  deallocate(nuc)
end program Mass_Distribution
