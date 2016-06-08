subroutine Random_walk(AtomValue)
  use parameters, only: gs,nuc,p1,p2,p3,p4,p5,lrzcs_0,d_lrzcs,&
       rcn,acn,n_lrzcs
  use command_args, only : E_excited
  implicit none
  real,intent(out) :: AtomValue
  real :: x,r0,r,l0,l
  integer(kind=selected_int_kind(1))::idirection
  integer(kind=selected_int_kind(1))::i
  integer,dimension(5)::ii
  logical(1)::recycle

  ii=gs!(/8,5,19,7,0/)!gs!(/8,5,19,7,0/)!(/15,12,16,15,9/)!(/8,5,19,7,0/)!(/13,10,21,12,7/)!(/15,12,16,15,9/)

  do
     l0=lrzcs_0(1)+d_lrzcs(1)*ii(1)
     r0=1/sqrt(l0)+ 0.05
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
!     i=i+1
!     if (i>1000000) exit
     r=(r0+ii(2)*d_lrzcs(2))
!     r=(r0+ii(2)*d_lrzcs(2))*Rcn*(Acn**(1./3.))
!     if (r<2.*Rcn) then
!     r=(r0+ii(2)*d_lrzcs(2))
     if (r<0.3) then
        write(*,*) 'random walk'
        write(*,*) ii
        AtomValue=nuc(ii(1),ii(2),ii(3),ii(4),ii(5))%getAH()
        exit
     endif
  enddo

contains

  subroutine Metropolis(INP_IDirection,ind,recycle)
    use quadrupole, only:cal_vbias
    implicit none

    integer(kind=selected_int_kind(1)), intent(in):: INP_IDirection
    integer, dimension(5),intent(inout) :: ind
    logical(1),intent(out)::recycle
    integer, dimension(5) :: tmp_ind
    real :: x, T
    T=sqrt((E_excited+nuc(gs(1),gs(2),gs(3),gs(4),gs(5))%getE()&
         -nuc(ind(1),ind(2),ind(3),ind(4),ind(5))%getE())/(Acn/8.))

    tmp_ind=ind
    recycle=.true.
    call random_number(x)
    if(x<=0.5) then
       tmp_ind(INP_IDirection)=ind(INP_IDirection)+1
    else
       tmp_ind(INP_IDirection)=ind(INP_IDirection)-1
    endif
    if (.not.exist(INP_IDirection,tmp_ind)) return
    if (E_exceed(tmp_ind)) return
    recycle=.false.
    
    call random_number(x)
    if(nuc(tmp_ind(1),tmp_ind(2),tmp_ind(3),tmp_ind(4),tmp_ind(5))%getE()+cal_vbias(tmp_ind)&
         <nuc(ind(1),ind(2),ind(3),ind(4),ind(5))%getE()+cal_vbias(ind).or.&
         x<exp((nuc(ind(1),ind(2),ind(3),ind(4),ind(5))%getE()+cal_vbias(ind)-&
         nuc(tmp_ind(1),tmp_ind(2),tmp_ind(3),tmp_ind(4),tmp_ind(5))%getE()+cal_vbias(tmp_ind))/T)) then
!!$    if(x<exp((nuc(ind(1),ind(2),ind(3),ind(4),ind(5))%getE()-&
!!$         nuc(tmp_ind(1),tmp_ind(2),tmp_ind(3),tmp_ind(4),tmp_ind(5))%getE())/T)) then
       ind(:)=tmp_ind(:)
    endif
  end subroutine Metropolis

  logical function exist(idirection,ii)
    implicit none
    integer(kind=selected_int_kind(1)),intent(in) :: IDirection
    integer,dimension(5),intent(in) :: ii
    integer :: n
    n=ii(idirection)
    if(n<=n_lrzcs(IDirection).and.n>=0) then
       exist=.true.
    else
       exist=.false.
    endif
  end function exist

  logical function E_exceed(ii)
    implicit none
    integer,dimension(5),intent(in) :: ii
    integer :: n
    if(nuc(ii(1),ii(2),ii(3),ii(4),ii(5))%getE()-&
         nuc(gs(1),gs(2),gs(3),gs(4),gs(5))%getE()>E_excited) then
       E_exceed=.true.
    else
       E_exceed=.false.
    endif
  end function E_exceed

end subroutine Random_walk
