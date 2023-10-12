!===========================================================================================================================
   module constants
!===========================================================================================================================
        IMPLICIT NONE
        real*8, parameter  :: pi = acos(-1.d0),pp = 4*pi*pi
        integer, parameter :: u = 100
              
   end module constants

!===========================================================================================================================
    program threebody
!===========================================================================================================================
! The Three-body Problem: 
! The Effect of Jupiter on Earth 
!---------------------------------------------------------------------------------------------------------------------------
        use constants
        IMPLICIT NONE
        real*8 pos_e(2),pos_j(2),pos_s(2),vel_e(2),vel_j(2),vel_s(2),me,mj,ms,pe,pj,dt
        integer nt

    !
    !   Open up the output file
    !
        open(u,file='output_3BodyCOM.dat')

        call initialize(pos_e,pos_j,pos_s,vel_e,vel_j,vel_s,ms,me,mj,pe,pj,dt,nt)
        call calculate(pos_e,pos_j,pos_s,vel_e,vel_j,vel_s,pe,pj,dt,nt)

    end program threebody

!===========================================================================================================================
   subroutine initialize(pos_e,pos_j,pos_s,vel_e,vel_j,vel_s,ms,me,mj,pe,pj,dt,nt)
!===========================================================================================================================
        use constants
        IMPLICIT NONE
        real*8 pos_e(*),pos_j(*),pos_s(*),vel_e(*),vel_j(*),vel_s(*),me,mj,ms,e_e,e_j,pj,pe,dt,r_cm,v_cm
        integer nt

    !
    !   Set up initial conditions (in AU units): 
    !
        open(1,file='input_3BodyCOM.dat')

        read(1,*) pos_e(1)    ! Earth initial position
        read(1,*) pos_j(1)    ! Jupiter initial position
        read(1,*) e_e         ! Earth eccentricity
        read(1,*) e_j         ! Jupiter eccentricity
        read(1,*) me          ! Earth mass
        read(1,*) mj          ! Jupiter mass
        read(1,*) ms          ! Sun mass
        read(1,*) dt          ! Time step
        read(1,*) nt          ! Number of time steps
        
        close(1)
    !
    !   Calculate the mass proportions with the sun
    !
        pj = mj/ms
        pe = me/ms
    !
    !   Write some initial info to the output file and the column titles
    !
        write(u,'(a,f10.6)') '# Time step:              ',dt
        write(u,'(a,f10.6)') '# Total time (years):     ',nt*dt 
        write(u,'(a,f10.6)') '# Jupiter-Sun mass ratio: ',pj
        write(u,'(a,f10.6)') '# Earth-Sun mass ratio:   ',pe
        write(u,'(a)') '#'
        write(u,'(a)') '#        t          x_earth       y_earth      x_jupiter     y_jupiter       x_sun         y_sun'
    !
    !   Calculate the initial positions of 3 bodies with the Sun initially at rest at the origin
    !
        pos_e(1) = pos_e(1)*(1-e_e)
        pos_e(2) = 0.d0
        vel_e(1) = 0.d0
        vel_e(2) = 2.d0*pi*sqrt((1+e_e)/abs(pos_e(1))/(1-e_e)*(1+pe))
        
        pos_j(1) = pos_j(1)*(1-e_j)
        pos_j(2) = 0.d0
        vel_j(1) = 0.d0    
        vel_j(2) = 2.d0*pi*sqrt((1+e_j)/abs(pos_j(1))/(1-e_j)*(1+pj))

        pos_s(1) = 0.d0
        pos_s(2) = 0.d0
        vel_s(1) = 0.d0
        vel_s(2) = 0.d0

    !   Compute the location and velocity of the center-of-mass

        r_cm = (me*pos_e(1) + mj*pos_j(1))/(me+mj+ms)
        v_cm = (me*vel_e(2) + mj*vel_j(2))/(me+mj+ms)
    !
    !   Transform to the COM rest frame with the COM at the origin    
    !
        pos_e(1) = pos_e(1) - r_cm
        pos_j(1) = pos_j(1) - r_cm
        pos_s(1) = pos_s(1) - r_cm
        vel_e(2) = vel_e(2) - v_cm
        vel_j(2) = vel_j(2) - v_cm
        vel_s(2) = vel_s(2) - v_cm

    !    write(u,'(a,f14.7)') '# Earth initial velocity:  ',vel_e(2)
    !    write(u,'(a,f14.7)') '# Jupiter initial velocity:',vel_j(2)
    !    write(u,'(a,f14.7)') '# Sun initial velocity:    ',vel_s(2)
        
   end subroutine initialize

!===========================================================================================================================
   subroutine calculate(pos_e,pos_j,pos_s,vel_e,vel_j,vel_s,pe,pj,dt,nt) 
!===========================================================================================================================
! use the Euler-Cromer method; update the velocities first
!---------------------------------------------------------------------------------------------------------------------------
        use constants
        IMPLICIT NONE
        real*8 me,mj,ms,pj,pe,pos_e(*),pos_j(*),pos_s(*),vel_e(*),vel_j(*),vel_s(*), t,dt,r_es,r_js,r_ej
        integer nt,i

        t = 0.0
    !
    !   Write the initial positions to the output file
    !
        write(u,'(10f14.7)')  t,pos_e(1),pos_e(2),pos_j(1),pos_j(2),pos_s(1),pos_s(2)

        do  i=1,nt
            r_es     = sqrt((pos_e(1)-pos_s(1))**2 + (pos_e(2)-pos_s(2))**2)
            r_js     = sqrt((pos_j(1)-pos_s(1))**2 + (pos_j(2)-pos_s(2))**2)
            r_ej     = sqrt((pos_e(1)-pos_j(1))**2 + (pos_e(2)-pos_j(2))**2)
    !
    !       Update velocities first
    !
            vel_e(1) = vel_e(1) - pp*dt*((pos_e(1)-pos_s(1))/r_es**3 + pj*(pos_e(1)-pos_j(1))/r_ej**3)
            vel_e(2) = vel_e(2) - pp*dt*((pos_e(2)-pos_s(2))/r_es**3 + pj*(pos_e(2)-pos_j(2))/r_ej**3)
            vel_j(1) = vel_j(1) - pp*dt*((pos_j(1)-pos_s(1))/r_js**3 + pe*(pos_j(1)-pos_e(1))/r_ej**3)
            vel_j(2) = vel_j(2) - pp*dt*((pos_j(2)-pos_s(2))/r_js**3 + pe*(pos_j(2)-pos_e(2))/r_ej**3)
            vel_s(1) = vel_s(1) - pp*dt*(pj*(pos_s(1)-pos_j(1))/r_js**3 + pe*(pos_s(1)-pos_e(1))/r_es**3)
            vel_s(2) = vel_s(2) - pp*dt*(pj*(pos_s(2)-pos_j(2))/r_js**3 + pe*(pos_s(2)-pos_e(2))/r_es**3)
    !
    !       Now update the postions using the new velocities
    !
            pos_e(1) = pos_e(1) + vel_e(1)*dt
            pos_e(2) = pos_e(2) + vel_e(2)*dt
            pos_j(1) = pos_j(1) + vel_j(1)*dt
            pos_j(2) = pos_j(2) + vel_j(2)*dt
            pos_s(1) = pos_s(1) + vel_s(1)*dt
            pos_s(2) = pos_s(2) + vel_s(2)*dt
        
            t        = t + dt
    !
    !       We don't want the output file to be too large. Writing every 10 steps should give enough points
    !            
            if(mod(i,10)==0) write(u,'(10f14.7)')  t,pos_e(1),pos_e(2),pos_j(1),pos_j(2),pos_s(1),pos_s(2)
        end do

   end subroutine calculate
