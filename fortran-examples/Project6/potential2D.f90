!===========================================================================================================================
    module subroutines
!===========================================================================================================================
        contains

            subroutine initialize(pot0,pot1,h,grid,imax,width,sep,nmax,u)

                IMPLICIT NONE
                real*8,allocatable,dimension(:,:),intent(out) :: pot0,pot1
                real*8 h
                integer grid,nmax,imax,width,sep,i,u
                
        
                open(1,file="input.dat")
        
                read(1,*) h         ! dx/scale of the problem
                read(1,*) grid      ! size of the grid (grid x grid)
                read(1,*) nmax      ! max number of iterations
        
                close(1)
        
                write(u,"(a)") "#      x             y          potential"
            !
            !   Determine the upper and lower bounds of the grid and allocate these pot0 and pot1
            !
                imax = grid/2
                allocate(pot0(-imax:imax,-imax:imax),pot1(-imax:imax,-imax:imax))
        
                pot0  = 0.d0
            !
            !   Make the capacitor plate width 1/10 of the grid size and their separation 1/2 of the width
            !
                width = imax/10
                sep   = width/2
                if(sep == 0) then
                    print *, "Grid too small"
                    stop
                endif
        
                do i=-width,width
                    pot0(sep,i)  = 1.d0
                    pot0(-sep,i) = -1.d0
                enddo
        
            end subroutine initialize
        

            subroutine calculate(pot_old,pot_new,h,imax,width,sep,error)

                IMPLICIT NONE
                real*8 h,error
                real*8,allocatable,dimension(:,:),intent(inout) :: pot_old,pot_new
                integer imax,width,sep,i,j
        
                error = 0.d0
                
                do i=1-imax,imax-1
                    do j=1-imax,imax-1
                        pot_new(i,j) = (pot_old(i,j+1) + pot_old(i,j-1) + pot_old(i+1,j) + pot_old(i-1,j))/4.d0
                        if(j <= width .AND. j >= -width) then
                            if(i==sep)  pot_new(i,j) = 1.d0
                            if(i==-sep) pot_new(i,j) = -1.d0
                        endif
                        error = error + abs(pot_old(i,j) - pot_new(i,j))
                    enddo
                enddo
        
            end subroutine calculate
                  
    end module subroutines

!============================================================================================================
    program potential2D
!============================================================================================================
!
!
!------------------------------------------------------------------------------------------------------------
        use subroutines
        IMPLICIT NONE
        real*8,allocatable,dimension(:,:) :: pot0,pot1
        real*8 h,error
        integer grid,niter,nmax,imax,width,sep,i,j
        integer, parameter :: u = 200
        real start_time, stop_time

        call cpu_time(start_time)

        open(u,file="output.dat")

        call initialize(pot0,pot1,h,grid,imax,width,sep,nmax,u)

        do niter=1,nmax
            call calculate(pot0,pot1,h,imax,width,sep,error)
            call calculate(pot1,pot0,h,imax,width,sep,error)

            if(error < 1E-05) exit
        enddo

        write(6,1000) 2*niter,error

        do i=-imax,imax
            do j=-imax,imax
                write(u,1001) h*i,h*j,pot0(i,j)
            enddo
            write(u,*)
        enddo

        1000    format("iterations: ",i5,"   error:",ES14.6)
        1001    format(10ES16.8)
        close(u)

        call cpu_time(stop_time)
        print *, 'Program finished in',stop_time-start_time,'seconds!'

    end program

