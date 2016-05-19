subroutine Random_walk(AtomValue)
  use omp_lib
  use parameters, only: gs,nuc,p1,p2,p3,p4,p5,lrzcs_0,d_lrzcs,&
       rcn,acn,idirection,n_lrzcs
  use command_args, only : E_excited
  implicit none
  real,intent(out) :: AtomValue
  real :: x,r0,r,l0,l
  integer,dimension(5)::ii
  ii=gs
  if (omp_get_thread_num()==3) then
     write(333,*) 'gs=',gs
     write(333,*) 'E_excited=',E_excited
  end if
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
     if (omp_get_thread_num()==3) then
        
        write(333,*) 'x=',x
        write(333,*) 'omp_get_thread_num()=',omp_get_thread_num()
        write(333,*) 'idirection:',idirection
        write(333,*) p1,p2,p3,p4,p5
     end if
     call Metropolis(IDirection,ii)
     r=(r0+ii(2)*d_lrzcs(2))*Rcn*(Acn**(1./3.))
     if (r<2.*Rcn) then
        write(*,*) 'random walk'
        AtomValue=nuc(ii(1),ii(2),ii(3),ii(4),ii(5))%AH
        exit
     endif
  enddo

contains

  subroutine Metropolis(INP_IDirection,ind)
    !    use quadrupole, only:cal_vbias
    implicit none

    integer, intent(in):: INP_IDirection
    integer, dimension(5),intent(inout) :: ind
    integer, dimension(5) :: tmp_ind
    integer :: i
    real :: x, T, test
    T=sqrt((E_excited+GetEnergyValue(gs)-GetEnergyValue(ind))/(Acn/8.))

    tmp_ind=ind
    call random_number(x)
    if(x<=0.5) then
       tmp_ind(INP_IDirection)=ind(INP_IDirection)+1
    else
       tmp_ind(INP_IDirection)=ind(INP_IDirection)-1
    endif
    if (omp_get_thread_num()==3) then
       write(333,*) 'in metropolis:'
       write(333,*) 'x=',x
       write(333,*) 'tmp_ind=',tmp_ind
    end if
    if (.not.exist(INP_IDirection,tmp_ind)) return
    if (E_exceed(tmp_ind)) return

    call random_number(x)
    !    if(x<exp((GetEnergyValue(ind)+Cal_Vbias(ind)-GetEnergyValue(tmp_ind)-Cal_Vbias(tmp_ind))/T)) then
    if(x<exp((GetEnergyValue(ind)-GetEnergyValue(tmp_ind))/T)) then
       ind(:)=tmp_ind(:)
       if (omp_get_thread_num()==3) then
          write(333,*) x<exp((GetEnergyValue(ind)-GetEnergyValue(tmp_ind))/T)
       end if
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

  logical function E_exceed(ii)
    implicit none
    integer,dimension(5),intent(in) :: ii
    integer :: n
    if(nuc(ii(1),ii(2),ii(3),ii(4),ii(5))%E-nuc(gs(1),gs(2),gs(3),gs(4),gs(5))%E>E_excited) then
       E_exceed=.true.
    else
       E_exceed=.false.
    endif
  end function E_exceed

end subroutine Random_walk
    
        
