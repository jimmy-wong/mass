real function GetValue(ind) 
	use Mod_Grid,only: UB_G,UB2,pG
	use Mod_Preparation,only: GridType
	implicit none
	integer,intent(in)	:: ind(5,2)
	type(GridType),pointer :: p					

	p => pG(ind(1,1),ind(2,1),ind(3,1),ind(4,1),ind(5,1))		! 
	if(.not.associated(p%G)) then
		GetValue = 10000.
	else
		GetValue = p%G(ind(1,2),ind(2,2),ind(3,2),ind(4,2),ind(5,2))%E		! 
	endif

end function 
