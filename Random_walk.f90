subroutine Random_walk(AtomValue)
  use Mod_Preparation, only : d_lrzcs,lrzcs_0,p1,p2,p3,p4,p5,ind0,output,Rcn,Acn
  use Mod_Grid, only:ind
  implicit none
  real,intent(out) :: AtomValue
  real :: x,r0,r,l0,l
  integer :: IDirection,i
  logical(1)::recycle

  ind=ind0
  i=0
  do
     l0=lrzcs_0(1)+d_lrzcs(1)*(ind(1,1)*2+ind(1,2))
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
     call Metropolis(IDirection,ind,recycle)
     if (recycle) cycle
     i=i+1
     if (i>1000000) exit
     r=(r0+(ind(2,1)*2+ind(2,2))*d_lrzcs(2))*Rcn*(Acn**(1./3.))
     l=l0*Rcn*(Acn**(1./3.))
     if (r<3.*Rcn) then
        AtomValue=GetAtomValue(ind)
write(*,*) r
        exit
     endif
  enddo

contains

  real function GetAtomValue(ind1) 
    use Mod_Grid,only: UB_G,UB2,pG
    use Mod_Preparation,only: GridType
    implicit none
    integer,intent(in)	:: ind1(5,2)
    type(GridType),pointer :: p					
    p => pG(ind1(1,1),ind1(2,1),ind1(3,1),ind1(4,1),ind1(5,1))		! 
    if(.not.associated(p%G)) then
       GetAtomValue = 10000.
    else
       GetAtomValue = p%G(ind1(1,2),ind1(2,2),ind1(3,2),ind1(4,2),ind1(5,2))%AH		! 
    endif
  end function GetAtomValue

end subroutine Random_walk
    
        
