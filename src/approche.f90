module approche

USE const_var
USE fonctions

contains

real(rp) function u_app(x,t)

	implicit none
	real(rp), intent(in) :: x,t
	real(rp) :: somme
	integer :: i, l
	
	somme = 0._rp
	do i = 1,2*M
		do l = 1,2*M
			somme = somme + U(i + (l-1)*(2*M))*(P_2i(i,x) - beta*P_2i(i,1._rp)/(1.+beta))*(P_2l(l,t)-t*P_2l(l,Tf)/Tf)
		end do
	end do
	u_app = somme + t*psi(x)/Tf +(1.-t/Tf)*phi(x)
	
	return

end function u_app

real(rp) function f_app(x)
	
	implicit none
	real(rp), intent(in) :: x
	real(rp) :: somme
	integer :: i
	
	somme= 0._rp
	do i = 1,2*M
		somme = somme + h_i(i,x)
	end do
	
	f_app = somme
	return
	
end function f_app
end module approche
