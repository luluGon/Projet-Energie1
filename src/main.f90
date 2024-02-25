program main

USE const_var
USE lecture_init
USE fonctions
USE creation_mat
USE mesh
USE Operations_matrices
USE approche


implicit none
integer :: i
call lecture()


allocate(Mat_A( (2*M)*(2*M+1) , (2*M)*(2*M+1) ) )
allocate( C( (2*M)*(2*M+1) )  )
allocate( U( (2*M)*(2*M+1) ) )
allocate(meshx(2*M),meshT(2*M+1))


CALL maillage()
mat_A(:,:) = 0._rp
CALL Construction_A()

CALL Construction_C()

CALL regul()

CALL gradconj(reg_A, reg_C, 2*M*(2*M+1), U, 100, 0.001_rp,ite)



write(6,*) ite

write(6,*) u_app(0.6_rp,0.2_rp )
write(6,*) f_app(
STOP

end program main
