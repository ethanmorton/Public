
!===========================================================================================================================
	program pendulum
!===========================================================================================================================
! Simulation of a non-linear driven pendulum
! Adapted from a program written by H. Nakanishi
!---------------------------------------------------------------------------------------------------------------------------
        IMPLICIT NONE
        integer, parameter :: u = 1
        integer nperiod,npp
	    real*8 phi(4),w(4),t(4),dt,kappa,q,Omega
        real start_time, stop_time

    ! Let's time this code

        call cpu_time(start_time)
    !
    !   Open the output file
    !

        open(unit = u, file = 'outputfile.dat')
    !
    ! For phi, w, and t, the 1st element will store the computed values to write to the output file.
    ! The other 3 elements store the middle steps used in the 4th-order Runge-Kutta method 
    !
        call initialize(t,phi,w,nperiod,npp,kappa,q,Omega,dt,u)
	    call calculate(phi,w,t,dt,kappa,q,Omega,npp,nperiod,u)

        call cpu_time(stop_time)
        print *, 'Program finished in',stop_time-start_time,'seconds!'

	end program pendulum

!===========================================================================================================================
    subroutine initialize(t,phi,w,nperiod,npp,kappa,q,Omega,dt,u)
!===========================================================================================================================
        IMPLICIT NONE
        real*8, parameter :: pi = acos(-1.d0)
        integer nperiod,npp,u
        real*8 t(*),phi(*),w(*),kappa,q,Omega,dt

        print *, 'Initializing Variables'
    !
    ! Read in initial values from the input file 
    !
        open(10,file='input.dat')
    
        read(10,*) phi(1)       ! initial angle --> phi
        read(10,*) w(1)         ! initial angular speed --> omega
        read(10,*) nperiod      ! number of periods
        read(10,*) npp          ! number of points per period
        read(10,*) kappa        ! damping constant
        read(10,*) q            ! force amplitude
        read(10,*) Omega        ! force frequency
    
        close(10)
    
        t(1) = 0.0d0
        dt   = pi/npp
    !
    ! Add initial values and column labels to the output file
    !    
        write(u,'(a,1pe14.6)') '# damping:               ',kappa
        write(u,'(a,1pe14.6)') '# drive amplitude        ',q
        write(u,'(a,1pe14.6)') '# drive frequency:       ',Omega
        write(u,'(a,1pe14.6)') '# dt:                    ',dt
        write(u,'(a,i10)') '# periods:            ',nperiod
        write(u,'(a,i10)') '# points per period:',npp
        write(u,'(a)') '#'
        write(u,1002)

    1002    format('#     time             phi             omega')

    end subroutine initialize

!===========================================================================================================================
    subroutine calculate(phi,w,t,dt,kappa,q,Omega,npp,nperiod,u)
!===========================================================================================================================
        IMPLICIT NONE
        real*8, parameter :: pi = acos(-1.d0)
        integer i,j,u,npp,nperiod
        real*8 phi(*),w(*),t(*),dw(4),dphi(4),dt,kappa,q,Omega

    !    write(1,1000) t(1),phi(1),w(1)
    !
    !   use 4th-order Runge-Kutta for high precision
    !
        do i = 1,nperiod    
            do j = 1,npp
                call deriv(w(1),phi(1),t(1),kappa,q,Omega,dw(1),dphi(1))
                t(2)   = t(1)+0.5d0*dt
                w(2)   = w(1)+0.5d0*dt*dw(1)
                phi(2) = phi(1)+0.5d0*dt*dphi(1)
                call deriv(w(2),phi(2),t(2),kappa,q,Omega,dw(2),dphi(2))
                t(3)   = t(2)
                w(3)   = w(1)+0.5d0*dt*dw(2)
                phi(3) = phi(1)+0.5d0*dt*dphi(2)
                call deriv(w(3),phi(3),t(3),kappa,q,Omega,dw(3),dphi(3))
                t(4)   = t(1)+dt
                w(4)   = w(1)+dt*dw(3)
                phi(4) = phi(1)+dt*dphi(3)
                call deriv(w(4),phi(4),t(4),kappa,q,Omega,dw(4),dphi(4))
                t(1)   = t(1)+dt
                w(1)   = w(1)+dt*(dw(1)+2.d0*dw(2)+2.d0*dw(3)+dw(4))/6.d0
                phi(1) = phi(1)+dt*(dphi(1)+2.d0*dphi(2)+2.d0*dphi(3)+dphi(4))/6.d0
            !
            ! Make sure phi stays in the range [-pi,pi]
            !
                if (phi(1)>pi) phi(1) = phi(1)-2.d0*pi
                if (phi(1)<-pi) phi(1) = phi(1)+2.d0*pi
            !    write(1,1000) t(1),phi(1),w(1)
            !
            ! Write to the output using a "stroboscope" measurement at the end
            ! of each period for the last 64 periods
            !
                if(i>nperiod-64.AND.j==npp) write(1,1000) t(1),phi(1),w(1)
            enddo
        enddo
        close(u)
    
    1000    format(10ES16.6E3)

	end subroutine calculate

!===========================================================================================================================
	subroutine deriv(w,phi,t,kappa,q,Omega,dw,dphi)
!===========================================================================================================================
        IMPLICIT NONE
        real*8 t,w,phi,kappa,q,Omega,dw,dphi

	    dphi = w
	    dw = -kappa*w - (1.d0+2.d0*q*cos(Omega*t))*sin(phi)

	end subroutine deriv
