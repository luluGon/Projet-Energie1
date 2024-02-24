module const_var

USE ISO_FORTRAN_ENV

IMPLICIT NONE

INTEGER, PARAMETER :: rp=REAL64
REAL(RP),PARAMETER :: pi = acos(-1.0_rp)


REAL(rp) :: a, epsi, Tf, mu
INTEGER :: info_u, info_phi, info_psi,M, ite

REAL(rp), dimension(:,:), allocatable :: Mat_A, reg_A, trans_A
REAL(rp), dimension(:), allocatable :: U,C, reg_C

end module const_var
