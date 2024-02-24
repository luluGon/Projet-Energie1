module Operations_matrices

! Dans ce module, nous avons nos défférentes opérations sur les matrices.
use const_var

contains

! méthode du gradient conjugué 
! argument 1 : matrice A (entrée)
! argument 2 : vecteur B (entrée)
! argument 3 : m, dimension B	(entrée)
! argument 4 : vecteur U (sortie)
! argument 5 : n, nombre d'itération maximal
! résultat U tel que AU=B
subroutine gradconj(A, B, m, U, ite_max, erreur,ite)
	implicit none
	real(rp), dimension(m,m), intent(in)	:: A
	real(rp), dimension(m), intent(in) 	:: B
	integer, intent(in) 			:: m, ite_max
	real(rp), intent(in)			:: erreur
	real(rp), dimension(m), intent(out)	:: U
	integer, intent(out)			:: ite
	
	real(rp), dimension(m)			:: R, P, AP, AU
	integer 				:: i
	real(rp)				:: alpha, beta, r0r0, r1r1
	
	
	U(:) = 1._rp
	AU = matmul(A,U)
	do i=1, m
		r(i) 	= B(i) - AU(i)
		U(i) 	= 0.0_rp
		p(i) 	= r(i)
	end do
	ite = 0
	do ite = 0, ite_max
		
		AP = matmul(A,p)
		r0r0 = dot_product(r,r)
		if ( abs(dot_product(p, AP) ) < epsilon(0._rp) ) then
			Write(6,*) "pas possible d'utiliser grad_conj"
			exit
		else
			alpha = r0r0/dot_product(p, AP)
			do i =1,m
				U(i) = U(i) + alpha*p(i)
				r(i) = r(i) - alpha*AP(i)
			end do
		
			r1r1 = dot_product(r,r)
		
			if ( r1r1< erreur*erreur) then
				exit
			end if
		
			beta = r1r1/r0r0
		
			do i=1, m
				p(i) = r(i) + beta*p(i)
			end do
		end if
		
		
	end do
	
	return 
	
end subroutine gradconj


subroutine regul()
	
	implicit none
	integer :: i
	
	trans_A = transpose(mat_A)
	reg_A = matmul(trans_A, mat_A)
	reg_C = matmul(trans_A, C)
	
	do i = 1, 4*M*M +2*M
		reg_A(i,i) = reg_A(i,i) + mu
	end do
	
	return

end subroutine regul

end module Operations_matrices
