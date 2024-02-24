MODULE mod_mesh

USE const_var

IMPLICIT NONE

CONTAINS

SUBROUTINE maillage(meshX,meshT)

IMPLICIT NONE
REAL(rp),DIMENSION(2*M+1),INTENT(OUT) :: meshX
REAL(rp),DIMENSION(2*M),INTENT(OUT) :: meshT

INTEGER r,s

DO r=2,2*M+1
	meshX(r-1)=-1.+(r-1.)/(M+2)
END DO

DO s=2,2*M+1
	meshT(s-1)=(s-1.)*Tf/(2.*M+1.)
END DO

RETURN

END SUBROUTINE maillage

END MODULE mod_mesh
