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
subroutine gradconj(A, B, m, U, n)
	implicit none
	real(rp), dimension(m,m), intent(in)	:: A
	real(rp), dimension(m), intent(in) 		:: B
	integer, intent(in) 					:: m,n
	real(rp), dimension(m), intent(out)		:: U
	
	real(rp), dimension(m)					:: R0,R1
	real(rp), dimension(m)					:: P0
	integer 								:: k, alpha_k, beta_k, i
	
	do i=1, m
		r0(i) 	= B(i) - matmul(A,U)
		U(i) 	= 0.0_rp
		p0(i) 	= r0(i)
	end do
	k = 0
	do while ( 
	
	
end subroutine gradconj

end module Operations_matrices
