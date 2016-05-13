module parameters
  implicit none 
  real :: p1,p2,p3,p4,p5,p

  real,dimension(5):: d_lrzcs, lrzcs_0, lrzcs
  integer,dimension(5):: n_lrzcs
  integer :: IDirection
  integer,dimension(5) :: gs
  real :: Acn, rcn, q0  
  integer :: output=10                    
  real::E_excited

  integer,parameter::dim_a=4
  real(8)::a(0:dim_a)				! shape parameters. coefficients in eq.(2.3) (general Lawrence shapes)

  type Gridtype
     real:: E=1.e6
     real:: AH
  end type GridType

  type(Gridtype),allocatable,dimension(:,:,:,:,:)::nuc

end module Parameters
    
