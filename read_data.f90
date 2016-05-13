subroutine read_the_data
  
  use command_args, only:command1
  use parameters
  use quadrupole, only: cal_quad
  use initial, only:init_params

  implicit none
  ! ______________________________________________________________________
  integer,dimension(5)::ii
  real,dimension(5):: shape_para
  real:: Edef, cm12, AH, Eld, Esh

  NameList /Grid/ d_lrzcs, n_lrzcs, lrzcs_0
  NameList /INPUT2/ gs, rcn, Acn
  integer:: IU = 10

  ! - dimensions -
  open(1,file='/home/zmwang/code/mass/Input/'//trim(command1)//'_Input.dat') 
  read(1,NML=Grid);	rewind(1)
  read(1,NML=INPUT2)
  close(1)
  allocate(nuc(0:n_lrzcs(1),0:n_lrzcs(2),0:n_lrzcs(3),0:n_lrzcs(4),0:n_lrzcs(5)))
  call init_params(n_lrzcs)
  q0=cal_quad(gs)
  ! - Construct the grid -
  open(IU,file='/home/zmwang/Documents/surface_result/Moller/'//trim(command1)//'.txt')

  read(IU,*)
  do 
     read(IU, *,end=100) cm12, ii(1:5), shape_para(1:5), Edef, Esh, Eld, AH
     nuc(ii(1),ii(2),ii(3),ii(4),ii(5))%E=Edef;
     nuc(ii(1),ii(2),ii(3),ii(4),ii(5))%AH=AH;
  enddo
100 continue

  close(IU)
  write(*,*) gs
  write(*,'(3(a,f10.3,5x))') 'E_excited=',E_excited, 'Acn=',Acn, 'Rcn=',Rcn

end subroutine Read_the_data
  
