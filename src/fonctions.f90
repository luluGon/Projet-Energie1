! Dans ce module nous retrouvons toutes les fonctions présent dans notre problème
module fonctions

use const_var
USE mod_mesh
contains 


! fonction qui correspond à u(x,t)
! arguments :
! x réel entre -1 et 1
! t réel entre 0 et T
! retourne u(x,t) 
! la fonction change selon le choix de info_u
! changeable dans le fichier init
real(rp) function u_ex(x,t)
	
	implicit none
	real(rp), intent(in) :: x, t
	
	select case (info_phi)
	
	case (0) 
	
		u_ex = 0.0_rp
		
	end select 
		
	return
	
end function u_ex

! la fonction phi(x) qui correspond à u(x,0)
! argument : x reel entre -1 et 1
! retourne phi(x)
! la fonction change selon le choix de info_phi
! changeable dans le fichier init
real(rp) function phi(x)

	implicit none
	real(rp), intent(in) :: x
	select case (info_phi)
	
		case (0)
			phi = 0.0_rp
			
	end select 
	
	return 
	
end function phi

! la fonction phi''(x) qui correspond à d²u/dx²(x,0)
! argument : x reel entre -1 et 1
! retourne phi''(x)
! la fonction change selon le choix de info_phi
! changeable dans le fichier init
real(rp) function phi_sec(x)

	implicit none
	real(rp), intent(in) :: x
	select case (info_phi)
	
		case (0)
			phi_sec = 0.0_rp
			
	end select 
	
	return 
	
end function phi_sec

! la fonction psi(x) qui correspond à u(x,T)
! argument : x reel entre -1 et 1
! retourne psi(x)
! la fonction change selon le choix de info_psi
! changeable dans le fichier init
real(rp) function psi(x)

	implicit none
	real(rp), intent(in) :: x
	select case (info_psi)
	
		case (0)
			psi = 0.0_rp
			
	end select 
	
	return 
	
end function psi

! fonction pour hi(x)
! argument 1 : i entier naturel
! argument 2 : x réel entre -1 et 1
! retourne hi(x)
real(rp) function h_i(i,x)
	implicit none
	integer, intent(in) :: i
	real(rp), intent(in) :: x
	
	integer :: m, k, ite, bitnum
	real(rp) :: epsi1, epsi2, epsi3
	logical :: bool
	
	
	! on définit la fonction selon la valeur de i
	select case (i)
	case (1)
		if ( x < 1._rp ) then
			h_i = 1._rp
		else 
			h_i = 0._rp
		endif
	
	case (2)
		if ( x < 0._rp ) then
			h_i = 1._rp
		elseif ( x >= 0._rp .and. x < 1._rp ) then
			h_i = -1._rp
		else 
			h_i = 0._rp
		end if
		
	case default 
		! calcul de k et m
		! nous allons travailler directement en binaire
		! on va chercher le 1 le plus à gauche dans l'écriture binaire
		bool = .false.
		ite = 0
		
		do while (bool .eqv. .false.) 
			bitnum = 31 - ite
			bool = btest(i-1, bitnum)
			ite = ite + 1
		end do
		
		m = ishft(1, bitnum) 
		k = i-1 - m
		
		! calcul de epsilon 1,2,3
		epsi1 = -1.0_rp + 2._rp*k/m
		epsi2 = -1.0_rp + 2._rp*(k + 0.5_rp)/m
		epsi3 = -1.0_rp + 2._rp*(k + 1.0_rp)/m
		
		if ( x >= epsi1 .and. x < epsi2 ) then 
			h_i = 1.0_rp
		elseif ( x >= epsi2 .and. x < epsi3 ) then
			h_i = -1.0_rp
		else 
			h_i = 0._rp
		end if
		
	end select
	return

end function h_i

! fonction pour hl(t)
! argument 1 : l entier naturel
! argument 2 : t réel entre 0 et T
! retourne hl(t)
real(rp) function h_l(l,t)
	implicit none
	integer, intent(in) :: l
	real(rp), intent(in) :: t
	
	integer :: m, k, ite, bitnum
	real(rp) :: epsi1, epsi2, epsi3
	logical :: bool
	
	! on définit la fonction selon la valeur de l
	select case (l)
	case (1)
		if ( t < T ) then
			h_l = 1._rp
		else 
			h_l = 0._rp
		endif
	
	case (2)
		if ( t < 0.5_rp ) then
			h_l = 1._rp
		elseif ( t >= 0.5_rp .and. t < T ) then
			h_l = -1._rp
		else 
			h_l = 0._rp
		end if
		
	case default 
		! calcul de k et m
		! nous allons travailler directement en binaire
		! on va chercher le 1 le plus à gauche dans l'écriture binaire
		bool = .false.
		ite = 0
		
		do while (bool .eqv. .false.) 
			bitnum = 31 - ite
			bool = btest(l-1, bitnum)
			ite = ite + 1
		end do
		
		m = ishft(1, bitnum) 
		k = l-1 - m
		
		
		! calcul de epsilon 1,2,3
		epsi1 = 0.0_rp + T*k/m
		epsi2 = 0.0_rp + T*(k + 0.5_rp)/m
		epsi3 = 0.0_rp + T*(k + 1.0_rp)/m
		
		if ( t >= epsi1 .and. t < epsi2 ) then 
			h_l = 1.0_rp
		elseif ( t >= epsi2 .and. t < epsi3 ) then
			h_l = -1.0_rp
		else 
			h_l = 0._rp
		end if
		
	end select
	return

end function h_l

! fonction P1,i(x)
! argument1 : i entier naturel 
! argument2 : x real entre -1 et 1
! renvoie P1,i(x) = integral de -1 à x de hi(y)dy
real(rp) function P_1i(i,x)
	implicit none
	integer, intent(in) :: i
	real(rp), intent(in) :: x
	integer :: m, k, ite, bitnum
	real(rp) :: epsi1, epsi2, epsi3
	logical :: bool
	
	! disjonction de cas selon la valeur de i
	select case (i)
	
	case (1)
		P_1i = (x + 1) ! x - a
		
	case default
		! calcul des epsilon 1,2,3
		bool = .false.
		ite = 0
		
		do while (bool .eqv. .false.) 
			bitnum = 31 - ite
			bool = btest(i-1, bitnum)
			ite = ite + 1
		end do
		
		m = ishft(1, bitnum) 
		k = i-1 - m
		
		epsi1 = -1.0_rp + 2._rp*k/m
		epsi2 = -1.0_rp + 2._rp*(k + 0.5_rp)/m
		epsi3 = -1.0_rp + 2._rp*(k + 1.0_rp)/m
		
		
			
		if ( x >= epsi1 .and. x < epsi2 ) then
			P_1i = x - epsi1
			
		elseif ( x >= epsi2 .and. x < epsi3 ) then
			P_1i = epsi3 - x
		else 
			P_1i = 0
		endif
	
	end select
		
	return
end function P_1i

! fonction P2,i(x)
! argument1 : i entier naturel 
! argument2 : x real entre -1 et 1
! renvoie P2,i(x) = integral de -1 à x de P_1i(y)dy
real(rp) function P_2i(i,x)
	implicit none
	integer, intent(in) :: i
	real(rp), intent(in) :: x
	integer :: m, k, ite, bitnum
	real(rp) :: epsi1, epsi2, epsi3
	logical :: bool
	
	! disjonction de cas selon la valeur de i
	select case (i)
	
	case (1)
		P_2i = 0.5_rp*(x+1)*(x+1) ! (x-a)²/2
		
	case default
		! calcul des epsilon 1,2,3
		bool = .false.
		ite = 0
		
		do while (bool .eqv. .false.) 
			bitnum = 31 - ite
			bool = btest(i-1, bitnum)
			ite = ite + 1
		end do
		
		m = ishft(1, bitnum) 
		k = i-1 - m
		
		epsi1 = -1.0_rp + 2._rp*k/m
		epsi2 = -1.0_rp + 2._rp*(k + 0.5_rp)/m
		epsi3 = -1.0_rp + 2._rp*(k + 1.0_rp)/m
		
		if ( x < epsi1) then
			P_2i = 0
			
		elseif ( x >= epsi1 .and. x < epsi2 ) then
			P_2i = 0.5_rp*(x-epsi1)*(x-epsi1)
			
		elseif ( x >= epsi2 .and. x < epsi3 ) then
			P_2i = (epsi3 -epsi2)**2 - 0.5_rp*(epsi3 - x)**2
		else 
			P_2i = (epsi3 -epsi2)**2
		endif
	
	end select
	
	return
end function P_2i


! fonction P_1l(t)
! argument1 : l entier naturel
! argument2 : t réel entre 0 et T
! retourne P_1l(t) = intérale de 0 à t de hl(y)dy
real(rp) function P_1l(l,t)
	implicit none
	integer, intent(in) :: l
	real(rp), intent(in) :: t
	integer :: m, k, ite, bitnum
	real(rp) :: epsi1, epsi2, epsi3
	logical :: bool
	
	! disjonction de cas selon la valeur de i
	select case (l)
	
	case (1)
		P_1l = t  ! t - a
		
	case default
		! calcul des epsilon 1,2,3
		bool = .false.
		ite = 0
		
		do while (bool .eqv. .false.) 
			bitnum = 31 - ite
			bool = btest(l-1, bitnum)
			ite = ite + 1
		end do
		
		m = ishft(1, bitnum) 
		k = l-1 - m
		
		epsi1 = 0.0_rp + T*k/m
		epsi2 = 0.0_rp + T*(k + 0.5_rp)/m
		epsi3 = 0.0_rp + T*(k + 1.0_rp)/m
		
		
			
		if ( t >= epsi1 .and. t < epsi2 ) then
			P_1l = t - epsi1
			
		elseif ( t >= epsi2 .and. t < epsi3 ) then
			P_1l = epsi3 - t
		else 
			P_1l = 0
		endif
		
	end select
	
	return
end function P_1l
!Source d'erreur possible: t et T interprété de la même façon

!Subroutine qui calcule A, la matrice de taille 2M(2M+1)x2M(2M+1) de notre système linéaire
SUBROUTINE Construction_A(Mat_A)

REAL(rp),DIMENSION(2*M*(2*M+1),2*M*(2*M+1)),INTENT(OUT) :: Mat_A

INTEGER :: K,L,i,PET_l,r,s
REAL(rp),DIMENSION(2*M+1) :: meshX
REAL(rp),DIMENSION(2*M) :: meshT

CALL maillage(meshX,meshT)

DO K=1,2*M*(2*M+1)
	!indice pour repérer les x_r
	r=1+(K-1)/(2*M)
	!indice pour repérer les t_s
	s=1+MOD(K-1,2*M)
	
	DO L=1,4*M**2
		!indice pour repérer les h_i et les P_2,i
		i=1+(L-1)/(2*M)
		!indice pour repérer les h_l et les P_1,l
		PET_l=1+MOD(L-1,2*M)
		Mat_A(K,L)=P_2i(i,meshX(r))*h_l(PET_l,meshT(s))+(h_i(i,-1._rp)+epsi*h_i(i,-meshX(r))-h_i(i,meshX(r)))*P_1l(PET_l,meshT(s))
	END DO
	DO L=1+4*M**2,2*M*(2*M+1)
		!indice pour repérer les h_i
		i=L-4*M**2
		Mat_A(K,L)=h_i(i,-1._rp)-h_i(i,meshX(r))
	END DO
END DO

RETURN

END SUBROUTINE Construction_A

!Subroutine qui calcule C, le vecteur de taille 2M(2M+1) de notre système linéaire
SUBROUTINE Construction_C(C)

REAL(rp),DIMENSION(2*M*(2*M+1)),INTENT(OUT) :: C

REAL(rp),DIMENSION(2*M+1) :: meshX
REAL(rp),DIMENSION(2*M) :: meshT
INTEGER :: L,r

CALL maillage(meshX,meshT)

DO L=1,2*M*(2*M+1)
	!indice pour repérer les x_r
	r=1+(L-1)/2*M
	C(L)=phi_sec(meshX(r))-phi_sec(-1._rp)+epsi*(phi_sec(1._rp)-phi_sec(-meshX(r)))
END DO

RETURN

END SUBROUTINE

end module fonctions
