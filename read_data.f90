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
  open(1,file='/home/zmwang/mass/Input/'//trim(command1)//'_Input.dat') 
  read(1,NML=Grid);	rewind(1)
  read(1,NML=INPUT2)
  close(1)
  allocate(nuc(0:n_lrzcs(1),0:n_lrzcs(2),0:n_lrzcs(3),0:n_lrzcs(4),0:n_lrzcs(5)))
  call init_params(n_lrzcs)
  q0=cal_quad(gs)
!  write(333,*) 'q0=',q0

  ! - Construct the grid -
  open(IU,file='/home/zmwang/mass/'//trim(command1)//'.txt')

  write(*,*) 'READING DATA'

  read(IU,*)
  do 
     read(IU, *,end=100) cm12, ii(1:5), shape_para(1:5), Esh, Eld, Edef, AH
     call nuc(ii(1),ii(2),ii(3),ii(4),ii(5))%setE(Edef);
     call nuc(ii(1),ii(2),ii(3),ii(4),ii(5))%setAH(AH);
  enddo
100 continue

  close(IU)
  write(*,'(a,5i3,a,a,f7.3)') 'ground_state=',gs,char(10),'ground_state_energy=',nuc(gs(1),gs(2),gs(3),gs(4),gs(5))%getE()
  write(*,'(3(a,f10.3,5x))') 'E_excited=',E_excited, 'Acn=',Acn, 'Rcn=',Rcn

end subroutine Read_the_data
