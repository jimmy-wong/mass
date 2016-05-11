module Mod_Preparation
  implicit none 
  integer,parameter :: R_G=5
  real :: p1,p2,p3,p4,p5,p                        !向各个方向游走的概率
  integer :: il,ir,iz,ic,is                       !各个方向的格点数
  real,dimension(5):: d_lrzcs, lrzcs_0, lrzcs     !lrzcs表示实际坐标位置
  integer,dimension(5):: n_lrzcs
  integer :: IDirection
  integer,dimension(5) :: ground_state
  integer,dimension(5,2) :: ind0
  real :: Acn, rcn, Q0  
  integer :: output=10                    

  type GridType
     real				  :: E=1.e6					! E>100 means E hasn't been calculated or E is greater than E_cut
     real                  :: AH
     type(GridType),pointer:: G(:,:,:,:,:)=>null()	! 
  end type GridType

contains

  subroutine Init_Mod_Preparation(INP_n_lrzcs)
    implicit none
    integer,dimension(5),intent(in)::INP_n_lrzcs
    n_lrzcs=INP_n_lrzcs
    il=(n_lrzcs(1)+1)
    ir=(n_lrzcs(2)+1)
    iz=n_lrzcs(3)+1
    ic=n_lrzcs(4)+1
    is=n_lrzcs(5)+1
    p=il**2+ir**2+iz**2+ic**2+is**2
    p1=il**2/p
    p2=p1+ir**2/p
    p3=p2+iz**2/p
    p4=p3+ic**2/p
    p5=p4+is**2/p
    ind0(:,1)=ground_state(:)/2
    ind0(:,2)=ground_state(:)-ind0(:,1)*2
  end subroutine Init_Mod_Preparation

end module Mod_Preparation
    
