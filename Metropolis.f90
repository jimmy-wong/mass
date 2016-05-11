subroutine Metropolis(INP_IDirection,ind,recycle)
use command_args,only :E_excited
  use Mod_Preparation, only : Acn, ind0
  implicit none

  integer, intent(in):: INP_IDirection
  integer, dimension(5,2),intent(inout) :: ind
  logical(1),intent(out)::recycle
  integer, dimension(5,2) :: tmp_ind
  integer :: i
  real :: x, T, test
  real,external::Cal_Vbias
  T=sqrt((E_excited+GetEnergyValue(ind0)-GetEnergyValue(ind))/(Acn/8.))

  tmp_ind=ind
  recycle=.true.
  call random_number(x)
  if(x<=0.5) then
     tmp_ind(INP_IDirection,1)=ind(INP_IDirection,1)*2+ind(INP_IDirection,2)+1
     tmp_ind(INP_IDirection,2)=mod(tmp_ind(INP_IDirection,1),2)
     tmp_ind(INP_IDirection,1)=tmp_ind(INP_IDirection,1)/2
  else
     tmp_ind(INP_IDirection,1)=ind(INP_IDirection,1)*2+ind(INP_IDirection,2)-1
     tmp_ind(INP_IDirection,2)=mod(tmp_ind(INP_IDirection,1),2)
     tmp_ind(INP_IDirection,1)=tmp_ind(INP_IDirection,1)/2
  endif
  if (.not.exist(INP_IDirection,tmp_ind)) return
  recycle=.false.

  call random_number(x)
  if(x<exp((GetEnergyValue(ind)+Cal_Vbias(ind)-GetEnergyValue(tmp_ind)-Cal_Vbias(tmp_ind))/T)) then
     ind(:,:)=tmp_ind(:,:)
  endif
contains

  real function GetEnergyValue(ind1) 
    use Mod_Grid,only: pG
    use Mod_Preparation,only: GridType
    implicit none
    integer,intent(in)	:: ind1(5,2)
    type(GridType),pointer :: p					

    p => pG(ind1(1,1),ind1(2,1),ind1(3,1),ind1(4,1),ind1(5,1))		! 
    if(.not.associated(p%G)) then
       GetEnergyValue = 10000.
    else
       GetEnergyValue = p%G(ind1(1,2),ind1(2,2),ind1(3,2),ind1(4,2),ind1(5,2))%E
    endif
  end function GetEnergyValue

  logical function exist(IDirection,ind1)
    use Mod_Preparation, only: n_lrzcs
    implicit none
    integer,intent(in) :: IDirection
    integer,dimension(5,2),intent(in) :: ind1
    integer :: n
    n=ind1(IDirection,1)*2+ind1(IDirection,2)
    if(n<=n_lrzcs(IDirection).and.n>=0) then
       exist=.true.
    else
       exist=.false.
    endif
  end function exist

end subroutine Metropolis
