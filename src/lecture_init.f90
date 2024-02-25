module lecture_init

use const_var
implicit none

contains 
subroutine lecture()

	open(unit=19, file='init', status='unknown', action='read')
		read(19,*) a
		read(19,*) beta
		read(19,*) epsi
		read(19,*) Tf
		read(19,*) info_u
		read(19,*) info_phi
		read(19,*) info_psi
		read(19,*) M
		read(19,*) mu
		close(19)
	end subroutine

end module lecture_init
