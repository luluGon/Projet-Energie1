MODULE mesh

USE const_var

IMPLICIT NONE

CONTAINS

SUBROUTINE maillage()

IMPLICIT NONE


INTEGER ::  r,s

DO r=1,2*M
	meshX(r)=-1.+r*2._rp/(2*M +1)
END DO

DO s=1,2*M+1
	meshT(s)=s*Tf/(2*M+2)
END DO

RETURN

END SUBROUTINE maillage

END MODULE mesh
