program main

USE const_var
USE lecture_init
USE fonctions
USE mod_mesh
USE Operations_matrices


implicit none

REAL(rp),DIMENSION(2*M*(2*M+1),2*M*(2*M+1)) :: Mat_A
REAL(rp),DIMENSION(2*M*(2*M+1)) :: C,U
INTEGER :: ite

call lecture()

CALL Construction_A(Mat_A)

CALL Construction_C(C)

CALL gradconj(Mat_A, C, 2*M*(2*M+1), U, 100, 0.001_rp,ite)

PRINT*,'U=',U

STOP

end program main
