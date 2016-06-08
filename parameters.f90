module parameters
  implicit none 
  real:: p1,p2,p3,p4,p5,p

  real,dimension(5):: d_lrzcs, lrzcs_0, lrzcs
  integer,dimension(5):: n_lrzcs
  integer,dimension(5):: gs
  
  real :: Acn, rcn, q0
  
  integer :: output=10                    
  real::E_excited

  type,public:: Gridtype
     real,private:: E=1.e6
     real,private:: AH
   contains
     procedure::setE => SetEnergyValue
     procedure::setAH => SetAtomValue
     procedure::getE => GetEnergyValue
     procedure::getAH => GetAtomValue
  end type GridType
  private::GetEnergyValue,GetAtomValue,SetEnergyValue,SetAtomValue

  class(Gridtype),allocatable,dimension(:,:,:,:,:)::nuc
 
  contains 
    subroutine SetEnergyValue(this,E)
      class(Gridtype)::this
      real::E
      this%E=E
    end subroutine SetEnergyValue
    subroutine SetAtomValue(this,AH)
      class(Gridtype)::this
      real:: AH
      this%AH=AH
    end subroutine SetAtomValue
    real function GetEnergyValue(this)
      class(Gridtype):: this
      GetEnergyValue=this%E
    end function GetEnergyValue
    real  function GetAtomValue(this)
      class(Gridtype)::this
      GetAtomValue=this%AH
    end function GetAtomValue

end module Parameters
