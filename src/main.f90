program main

USE const_var
USE lecture_init
USE fonctions
USE mod_mesh
USE Operations_matrices



implicit none
integer :: i
call lecture()


allocate(Mat_A( (2*M)*(2*M+1) , (2*M)*(2*M+1) ) )
allocate( C( (2*M)*(2*M+1) )  )
allocate( U( (2*M)*(2*M+1) ) )



CALL Construction_A()

CALL Construction_C()

CALL regul()

CALL gradconj(reg_A, reg_C, 2*M*(2*M+1), U, 100, 0.001_rp,ite)


do i=1, (2*M)*(2*M+1)
	Write(6,*) C(i)
end do


STOP

end program main
