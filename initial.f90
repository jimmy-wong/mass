module initial
  use parameters, only: n_lrzcs,p1,p2,p3,p4,p5

contains
  subroutine init_params(n_lrzcs)
    implicit none
    integer,dimension(5),intent(in)::n_lrzcs
    integer :: il,ir,iz,ic,is
    real::p
    il=n_lrzcs(1)+1;ir=n_lrzcs(2)+1;iz=n_lrzcs(3)+1;ic=n_lrzcs(4)+1;is=n_lrzcs(5)+1
    p=il**2+ir**2+iz**2+ic**2+is**2
    p1=il**2/p
    p2=p1+ir**2/p
    p3=p2+iz**2/p
    p4=p3+ic**2/p
    p5=p4+is**2/p
  end subroutine init_params
end module initial
