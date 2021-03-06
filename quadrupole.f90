Module quadrupole
  use parameters, only:lrzcs_0,d_lrzcs,q0    
  implicit none
Contains

  ! Calculates the technical shape parameters a(0:4) from the geometrical
  !	ones - l, r, z, c, s.
  ! The input arguments l, r, z, c, s are measured as multiples of Rcn. 
  ! The output arguments l, lrzcs are measured by fermi
  ! For using the parameters measured in fermi:
  !		(l, r, z, c, s) -> (l, r, z, c, s)*Rcn
  !				a(n)	->	a(n)/Rcn**n		n=0,1,2,3,4
  subroutine cal_a (lrzcs0,a)
    implicit none

    real,intent(in),dimension(5):: lrzcs0
    real,intent(out),dimension(0:4)::a
    real(8) term0,term1,term2,term3,term4				! help variable

    real(8) l2,l3,l4,l5,l6,l7,z2,z3,z4,z5,z6
    real(8) l,r,z,c,s
    integer n,i
    logical b1,b2

    l = lrzcs0(1)
    r = lrzcs0(2)
    z = lrzcs0(3)
    c = lrzcs0(4)
    s = lrzcs0(5)
    l2=l*l;		l3=l2*l;	l4=l2*l2;	l5=l3*l2;	l6=l3*l3;	l7=l3*l4;	
    z2=z*z;		z3=z2*z;	z4=z2*z2;	z5=z3*z2;	z6=z3*z3;

    term0=l2-z2
    a(0)=r*r/term0
    a(1)=2.*a(0)*z/term0
    a(2)=(c*r+a(0)+2.*a(1)*z)/term0
    term1=s*(15.*l4+210.*l2*z2+175.*z4) &
         +(1-a(0)*l3)*(60.*l4*z+140.*l2*z3) &
         -a(1)*(3.*l4-18.*l2*z2-105.*z4)*l5 &
         -a(2)*(6.*l4*z+4.*l2*z3+70.*z5)*l5
    term2=(9.*l6/7.-9.*l4*z2-3.*l2*z4-35.*z6)*l5
    a(3)=term1/term2

    term1=1-a(0)*l3+a(1)*l3*z-a(2)*(l3*z2+0.2*l5)+a(3)*(l3*z3+0.6*l5*z)
    term2=l3*z4+1.2*l5*z2+3*l7/35
    a(4)=term1/term2

    ! - geometrical shape parameters: l,r,z,c,s
    ! - generalized Lawrence shape parameters: a(i), i=0,1,2,3,4

  end subroutine cal_a

  subroutine cal_grid(ii,lrzcs0,lrzcs_out)
    implicit none
    integer,dimension(5),intent(in) :: ii
    real,dimension(5),intent(in) :: lrzcs0
    real,dimension(5),intent(out) ::lrzcs_out
    real,dimension(5)::lrzcs
    integer ::i1,i2,i3,i4,i5 

    i1=ii(1);i2=ii(2);i3=ii(3);i4=ii(4);i5=ii(5)

    lrzcs_out(1) = lrzcs0(1) + i1*d_lrzcs(1)
    lrzcs_out(2) = 1/sqrt(lrzcs_out(1)) + 0.05 + i2*d_lrzcs(2)
    lrzcs_out(3) = lrzcs0(3) + i3*d_lrzcs(3)
    lrzcs_out(4) = -1/lrzcs_out(1)**2.5 - 0.5 + i4*d_lrzcs(4)
    lrzcs_out(5) = lrzcs0(5) + i5*d_lrzcs(5)

  end subroutine cal_grid


  real function cal_quad(ii)

    implicit none

    integer,dimension(5),intent(in):: ii
    real :: l,r,z,s,c
    real,dimension(0:4)::a
    real,dimension(5)::lrzcs
    call cal_grid(ii,lrzcs_0,lrzcs)                !calculate the real value of lrzcs
    call cal_a(lrzcs,a)                           !calculate the parameters a0,a1,a2,a3,a4
 !   write(333,'(a,5f5.2)') 'a=',a
 !   write(333,'(a,5f5.2)') 'lrzcs=',lrzcs

    l=lrzcs(1)
    r=lrzcs(2)
    z=lrzcs(3)
    c=lrzcs(4)
    s=lrzcs(5)

    cal_quad= &
         -1/45045.*4.*l**5*(3003.*a(0)**2 - 1287.*a(2)*l**2 + 143.*a(2)**2*l**4 - &
         715.*a(4)*l**4 + 65.*a(3)**2*l**6 + 130.*a(2)*a(4)*l**6 + 35.*a(4)**2*l**8 + & 
         3861.*a(3)*l**2*z - 1430.*a(2)*a(3)*l**4*z - 910.*a(3)*a(4)*l**6*z - &
         3003.*a(2)*z**2 + 2574.*a(2)**2*l**2*z**2 - 7722.*a(4)*l**2*z**2 + &
         2145.*a(3)**2*l**4*z**2 + 4290.*a(2)*a(4)*l**4*z**2 + 1820.*a(4)**2*l**6*z**2 + &
         3003.*a(3)*z**3 - 8580.*a(2)*a(3)*l**2*z**3 - 10010.*a(3)*a(4)*l**4*z**3 + &
         3003.*a(2)**2*z**4 - 3003.*a(4)*z**4 + 6425.*a(3)**2*l**2*z**4 + &
         12870.*a(2)*a(4)*l**2*z**4 + 10010.*a(4)**2*l**4*z**4 - 6006.*a(2)*a(3)*z**5 - &
         18018.*a(3)*a(4)*l**2*z**5 + 3003.*a(3)**2**6 + 6006.*a(2)*a(4)*z**6 + &
         12012.*a(4)**2*l**2*z**6 - 6006.*a(3)*a(4)*z**7 + 3003.*a(4)**2*z**8 + &
         3429.*a(1)**2*(l**2 + 7.*z**2) + &
         143.*a(0)*(6.*a(2)*(l**2 + 7.*z**2) - &
         3.*(7. + 14.*a(1)*z + 6.*a(3)*l**2*z + 14.*a(3)*z**3) + &
         2.*a(4)*(l**4 + 18.*l**2*z**2 + 21.*z**4)) + &
         143.*a(1)*(2.*a(3)*(l**4 + 18.*l**2*z**2 + 21.*z**4) - &
         z*(-21. + 10.*a(4)*l**4+60.*a(4)*l**2*z**2+42.*a(4)*z**4+6.*a(2)*(3*l**2+7*z**2))))

  end function cal_quad

  real function cal_vbias(ind1)
    implicit none

    integer,dimension(5),intent(in) :: ind1
    real,parameter :: V0=15.                         !V0=15MeV
    real :: q
    q=cal_quad(ind1)
    cal_vbias=v0*(q0/q)**2
!    write(333,*) 'q=',q
!    write(333,*) 'ii=',ind1
!    write(333,*) 'cal_vbias=',cal_vbias
  end function cal_vbias

end module quadrupole
