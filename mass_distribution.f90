program Mass_Distribution
  use omp_lib
  use command_args,only:init_command_args,command1
  use parameters, only: gs,nuc,n_lrzcs,Q0,output

  implicit none
  external :: read_the_data

  real :: atomvalue
  integer :: i_random_walk,i_counts
  integer,dimension(5,2)::ind

  call init_command_args
  i_counts=0
!  open(50,file='/home/zmwang/Documents/mass_result/'//trim(command1)//'.txt',status='replace',action='write')
  open(50,file='/home/zmwang/mass/'//trim(command1)//'_output.txt',action='write',asynchronous='yes')
  call read_the_data

  call random_seed()

  call omp_set_num_threads(24)
!$omp parallel do &
!$omp private(i_random_walk,atomvalue) 
  do i_random_walk = 1,10000
     call Random_walk(atomvalue)
     write(50,'(4x,f10.5)') atomvalue
     write(*,'(4x,f10.5)') atomvalue
  enddo
!$omp end parallel do
  close(50)
  deallocate(nuc)
end program Mass_Distribution
