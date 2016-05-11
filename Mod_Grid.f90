Module Mod_Grid
  use Mod_Preparation, only:R_G,Gridtype,d_lrzcs,lrzcs_0,n_lrzcs
  implicit none

  integer:: Dim_G(R_G,2)
  integer:: ind(5,2)
  integer:: UB_G(R_G,2)					! upper bound. Lower bounds are set zero.
  integer:: UB1(R_G),UB2(R_G)				! upper bound


  type(GridType),pointer:: p_Root			! points to root node
  type(GridType),pointer:: pG(:,:,:,:,:)	! points to 1st Level Grid

contains

  ! ¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€¨€
  subroutine Init_Mod_Grid(IU,INP_lrzcs_0, INP_d_lrzcs, INP_n_lrzcs)
    implicit none
    integer,intent(in):: IU
    real,intent(in):: INP_d_lrzcs(5), INP_lrzcs_0(5)
    integer,intent(in):: INP_n_lrzcs(5)

    type(GridType),pointer:: p

    integer:: i1,i2,i3,i4,i5,i,ii(5)
    integer:: j1,j2,j3,j4,j5,j,jj(5)
    integer:: n,ix

    real:: shape_para(5)
    real:: Edef, cm12, AH, Eld, Esh

    type(GridType),allocatable::Grid(:,:,:,:,:)

    d_lrzcs(:) = INP_d_lrzcs(:)
    lrzcs_0(:) = INP_lrzcs_0(:) 
    n_lrzcs(:) = INP_n_lrzcs(:)

    Dim_G(:,2 ) = 2
    Dim_G(:,1 ) = n_lrzcs(:)/Dim_G(:,2 )+1

    UB_G(:,:) = Dim_G(:,:) - 1

    UB1(:) = UB_G(:,1)				! upper bounds
    UB2(:) = UB_G(:,2)				! upper bounds

    ! ______________________________________________________________________
    ! set up grid

    allocate( p_Root )
    allocate( p_Root%G( 0:UB1(1), 0:UB1(2), 0:UB1(3), 0:UB1(4), 0:UB1(5)) )
    pG => p_Root%G
    pG(:,:,:,:,:)%E = 1000.
    pG(:,:,:,:,:)%AH = 1000.

    ! ______________________________________________________________________
    !@	Nodes(:)
    ! create an array containing pointers associated with each low enengy node on L2 grid.

    read(IU, *)
    read(IU,*)
    do 
       read(IU, *,end=100) cm12, ii(1:5), shape_para(1:5), Edef, Esh, Eld, AH

       ind(:,1) = ii(:)/Dim_G(:,2)
       ind(:,2) = mod(ii(:),Dim_G(:,2))
       i1 = ind(1,1);		j1 = ind(1,2); 
       i2 = ind(2,1);		j2 = ind(2,2); 
       i3 = ind(3,1);		j3 = ind(3,2); 
       i4 = ind(4,1);		j4 = ind(4,2); 
       i5 = ind(5,1);		j5 = ind(5,2); 

       if(ALL(ind(:,2)==0)) then
          pG(i1,i2,i3,i4,i5)%E = Edef
          pG(i1,i2,i3,i4,i5)%AH = AH
       endif
       p => pG(i1,i2,i3,i4,i5)			! node on 1st grid
       if(.not.associated(p%G)) then
          allocate( p%G( 0:UB2(1), 0:UB2(2), 0:UB2(3), 0:UB2(4), 0:UB2(5)))
          p%G(:,:,:,:,:)%E = 10000.
          p%G(:,:,:,:,:)%AH = 10000.
       endif
       p%G(j1,j2,j3,j4,j5)%E = Edef
       p%G(j1,j2,j3,j4,j5)%AH = AH

       ii(:) = (/i1,i2,i3,i4,i5/)
       jj(:) = (/j1,j2,j3,j4,j5/)

    enddo
100 continue
  end subroutine Init_Mod_Grid

  subroutine Cal_Grid(ind1,lrzcs0,ARG_lrzcs0)
    implicit none
    integer,dimension(5,2),intent(in) :: ind1
    real,dimension(5),intent(in) :: lrzcs0
    real,dimension(5),intent(out) :: ARG_lrzcs0
    real,dimension(5)::lrzcs
    integer ::i1,i2,i3,i4,i5 

    lrzcs=lrzcs0
    i1=ind1(1,1)*2+ind1(1,2)
    i2=ind1(2,1)*2+ind1(2,2)
    i3=ind1(3,1)*2+ind1(3,2)
    i4=ind1(4,1)*2+ind1(4,2)
    i5=ind1(5,1)*2+ind1(5,2)

    ARG_lrzcs0(1) = lrzcs(1) + i1*d_lrzcs(1)
    lrzcs(2) = 1/sqrt(ARG_lrzcs0(1))
    ARG_lrzcs0(2) = lrzcs(2) + i2*d_lrzcs(2)
    ARG_lrzcs0(3) = lrzcs(3) + i3*d_lrzcs(3)
    lrzcs(4) = -1/ARG_lrzcs0(1)**2.5
    ARG_lrzcs0(4) = lrzcs(4) + i4*d_lrzcs(4)
    ARG_lrzcs0(5) = lrzcs(5) + i5*d_lrzcs(5)

  end subroutine Cal_Grid

end module Mod_Grid
