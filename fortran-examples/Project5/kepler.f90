!=============================================================================================
    module constants
!=============================================================================================
        IMPLICIT NONE
        real*8, parameter  :: pi = acos(-1.d0),pp = 4*pi*pi
        integer, parameter :: u = 100
        
    end module constants

!=============================================================================================
    program kepler
!=============================================================================================
!  Planetary motion (simplified circle) 
!---------------------------------------------------------------------------------------------
        use constants
        IMPLICIT NONE
        real*8 x,y,vx(2),vy(2),t,dt,phi,phi_sum,a,b,e
        integer n,euler
        

        open(u, file = 'input_kep.dat')

        call initialize(x,y,vx,vy,t,dt,phi,phi_sum,a,b,e,n,euler)

        call calculate(x,y,vx,vy,t,dt,phi,phi_sum,b,n,euler)

    end program kepler

!=============================================================================================
    subroutine initialize(x,y,vx,vy,t,dt,phi,phi_sum,a,b,e,n,euler)
!=============================================================================================
        use constants
        IMPLICIT NONE
        real*8 x,y,vx(*),vy(*),t,dt,phi,phi_sum,a,b,e
        integer n,euler,manual

    !   Read initial conditions.  We always start on x-axis with non-zero vy.

        read(u,*) dt        ! Time steps
        read(u,*) n         ! Number of orbits
        read(u,*) e         ! Eccentricity
        read(u,*) a         ! Semimajor axis
        read(u,*) b         ! Beta
        read(u,*) euler     ! Toggle for Euler method
        read(u,*) manual    ! Toggle to manually set initial values

        t       = 0.d0
        phi     = 0.d0
        phi_sum = 0.d0
        
        y  = 0.d0; vx(1) = 0.d0             ! We always start on the x-axisi, so vx = 0.0d0 
        x  = a*(1.d0-e)                     ! Starting position for elliptical orbit  
    !    vy = 2.d0*pi/sqrt(a)                ! This is v_y for circular orbits  
        vy(1) = 2*pi*sqrt((1+e)/a/(1-e))    ! This is v_y for elliptical orbits (M_s/M_p = 0)

        if(manual == 1) then
            read(u,*) x         
            read(u,*) vy(1)
        endif

        close(u)

    end subroutine initialize

!=============================================================================================
    subroutine calculate(x,y,vx,vy,t,dt,phi,phi_sum,b,n,euler)
!=============================================================================================
        use constants
        IMPLICIT NONE
        real*8 x,y,vx(*),vy(*),t,dt,phi,phi_sum,phi_new,r,b
        integer n,euler

        !   Calculations and output: 

        open(2,file = 'output_kep.dat',status='unknown')
        write(2,1000)

        !
        !   Main time loop
        !
        do
            write(2,1001) t,x,y,vx(1),vy(1),phi_sum
            r       = sqrt(x**2 + y**2)
            vx(2)   = vx(1) - (pp*x*dt)/r**(b+1)
            vy(2)   = vy(1) - (pp*y*dt)/r**(b+1)
            
            if(euler==1) then 
                ! Use Euler step  
                x   = x + vx(1)*dt
                y   = y + vy(1)*dt
            else
                ! Use Euler-Cromer step
                x   = x + vx(2)*dt
                y   = y + vy(2)*dt
            endif

            vx(1)   = vx(2)
            vy(1)   = vy(2)
            phi_new = atan2(abs(y),abs(x))
            phi_sum = phi_sum + abs(phi_new-phi)
            phi     = phi_new 
            t       = t + dt
            if(phi_sum>=2*pi*n) stop
        enddo

        write(2,1001) t,x,y,vx(1),vy(1),phi_sum
        1000  format('#      t             x             y            vx            vy         phi-total')
        1001  format(1p10e14.6)

        close(2)

        end subroutine calculate