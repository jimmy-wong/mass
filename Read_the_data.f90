subroutine Read_the_data
  use command_args, only:command1,E_excited
  use Mod_Preparation, only :d_lrzcs, n_lrzcs, lrzcs_0, ind0, Q0,ground_state, Rcn,Acn,Init_Mod_Preparation      
  use Mod_Grid	,only: Init_Mod_Grid
  use Mod_Shape, only: cal_quadropole

  implicit none
  ! ______________________________________________________________________

  NameList /Grid/ d_lrzcs, n_lrzcs, lrzcs_0
  NameList /INPUT2/ ground_state, rcn, Acn
  integer:: IU = 10

  ! - dimensions -
  open(1,file='/home/zmwang/code/mass/Input/'//trim(command1)//'_Input.dat') 
  read(1,NML=Grid);	rewind(1)
  read(1,NML=INPUT2)
  close(1)
  call Init_Mod_Preparation(n_lrzcs)
  Q0=cal_quadropole(ind0)
  ! - Construct the grid -
  open(IU,file='/home/zmwang/Documents/surface_result/Moller/'//trim(command1)//'.txt')
  call Init_Mod_Grid  (IU,lrzcs_0, d_lrzcs, n_lrzcs)
  close(IU)
  write(*,*) ind0
  write(*,'(3(a,f10.3,5x))') 'E_excited=',E_excited, 'Acn=',Acn, 'Rcn=',Rcn

end subroutine Read_the_data


    
    
