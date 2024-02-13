MODULE mod_mesh

USE const_var

IMPLICIT NONE

CONTAINS

SUBROUTINE maillage(meshX,meshT)

REAL(rp),DIMENSION(2*M+1),INTENT(OUT) :: meshX
REAL(rp),DIMENSION(2*M),INTENT(OUT) :: meshT

INTEGER r,s

DO r=1,2*M+1
	meshX(r)=-1.+(r-1.)/M
END DO

DO s=1,2*M
	meshT(s)=(s-1.)*T/(2.*M-1.)
END DO

RETURN

END SUBROUTINE maillage

END MODULE mod_mesh
