module parameters
  implicit none 
  real:: p1,p2,p3,p4,p5,p

  real,dimension(5):: d_lrzcs, lrzcs_0, lrzcs
  integer,dimension(5):: n_lrzcs
  integer,save :: IDirection
  integer,dimension(5):: gs
  
  real,save :: Acn, rcn, q0
  
  integer :: output=10                    
  real::E_excited
!$omp threadprivate(idirection)
  type Gridtype
     real:: E=1.e6
     real:: AH
  end type GridType

  type(Gridtype),allocatable,dimension(:,:,:,:,:)::nuc

end module Parameters
    
