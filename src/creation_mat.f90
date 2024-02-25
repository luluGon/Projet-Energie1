module creation_mat

USE const_var
USE fonctions

contains

! création de la matrice A
subroutine construction_A()

	implicit none
	integer :: i,l,r,s
	mat_A(:,:) = 0._rp
	do s=1,2*M+1
		do r=1,2*M
			do i=1,2*M
				do l=1,2*M
					mat_A(r +(s-1)*(2*M+1), l +(i-1)*2*M) = R_il(r,s,i,l)
				end do
				mat_A(r +(s-1)*(2*M), 4*M*M +i) = -h_i(i, meshx(r))
			end do
		end do
	end do
	return
end subroutine construction_A

! création vecteur C
subroutine construction_C
	
	implicit none
	real(rp) :: x,t 
	integer :: r,s
	c(:) = 0._rp
	do s=1, 2*M+1
		do r=1, 2*M
			x = meshx(r)
			t = mesht(s)
			C(r + (s-1)*(2*M) ) = ( phi(x) - psi(x))/Tf + t*psi_sec(x)/Tf + (1-t/Tf)*phi_sec(x) - &
			& epsi*((1-t/Tf)*phi_sec(-x) -t*psi_sec(-x))/Tf
		end do
	end do
	
	return
end subroutine construction_C

end module creation_mat
