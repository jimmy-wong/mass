subroutine Random_walk(AtomValue)
  use parameters
  implicit none
  real,intent(out) :: AtomValue
  real :: x,r0,r,l0,l
  integer :: i
  logical(1)::recycle
  integer,dimension(5)::ii
  ii=gs
  i=0
  do
     l0=lrzcs_0(1)+d_lrzcs(1)*ii(1)
     r0=1/sqrt(l0) 
     call random_number(x)
     if (x<p1) then
        IDirection=1
     elseif(x<p2) then
        IDirection=2
     elseif(x<p3) then
        IDirection=3
     elseif(x<p4) then
        IDirection=4
     elseif(x<p5) then
        IDirection=5
     endif
     call Metropolis(IDirection,ii,recycle)
     if (recycle) cycle
     i=i+1
     if (i>1000000) exit
     r=(r0+ii(2)*d_lrzcs(2))*Rcn*(Acn**(1./3.))
     l=l0*Rcn*(Acn**(1./3.))
     if (r<3.*Rcn) then
        AtomValue=nuc(ii(1),ii(2),ii(3),ii(4),ii(5))%AH
	write(*,*) r
        exit
     endif
  enddo

contains

  subroutine Metropolis(INP_IDirection,ind,recycle)
    use quadrupole, only:cal_vbias
    implicit none

    integer, intent(in):: INP_IDirection
    integer, dimension(5),intent(inout) :: ind
    logical(1),intent(out)::recycle
    integer, dimension(5) :: tmp_ind
    integer :: i
    real :: x, T, test
    T=sqrt((E_excited+GetEnergyValue(gs)-GetEnergyValue(ind))/(Acn/8.))

    tmp_ind=ind
    recycle=.true.
    call random_number(x)
    if(x<=0.5) then
       tmp_ind(INP_IDirection)=ind(INP_IDirection)+1
    else
       tmp_ind(INP_IDirection)=ind(INP_IDirection)-1
    endif
    if (.not.exist(INP_IDirection,tmp_ind)) return
    recycle=.false.

    call random_number(x)
    if(x<exp((GetEnergyValue(ind)+Cal_Vbias(ind)-GetEnergyValue(tmp_ind)-Cal_Vbias(tmp_ind))/T)) then
       ind(:)=tmp_ind(:)
    endif
  end subroutine Metropolis

  real function GetEnergyValue(ii) 
    use parameters, only:nuc
    implicit none
    integer,intent(in),dimension(5)::ii
    GetEnergyValue=nuc(ii(1),ii(2),ii(3),ii(4),ii(5))%E
  end function GetEnergyValue

  logical function exist(IDirection,ii)
    implicit none
    integer,intent(in) :: IDirection
    integer,dimension(5),intent(in) :: ii
    integer :: n
    n=ii(IDirection)
    if(n<=n_lrzcs(IDirection).and.n>=0) then
       exist=.true.
    else
       exist=.false.
    endif
  end function exist



end subroutine Random_walk
    
        
