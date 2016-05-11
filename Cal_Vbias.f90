    real function Cal_Vbias(ind1)
    use Mod_Preparation, only : Q0
    use Mod_Shape, only : Cal_Quadropole
    implicit none
    
    integer,dimension(5,2),intent(in) :: ind1
    real,parameter :: V0=10.                         !V0=15MeV
    real :: Q
    !real :: term
    Q=Cal_Quadropole(ind1)
    Cal_Vbias=V0*(Q0/Q)**2
    !term=Cal_Vbias
    end function
    
    
    
