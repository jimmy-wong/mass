module command_args
  use parameters, only : E_excited
  implicit none
  integer::length, status
  character(len=10)::command1,command2,command3,command5
  character(len=140)::command4

contains
subroutine init_command_args
  call get_command_argument(1,command1,length,status)
  call get_command_argument(2,command2,length,status)
  call get_command_argument(3,command3,length,status)
  call get_command_argument(4,command4,length,status)
  call get_command_argument(5,command5,length,status)
  read(command2,*) E_excited
end subroutine init_command_args
end module
