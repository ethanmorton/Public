!================================================================================================================================
  Program Diagonalization
!================================================================================================================================
    implicit none
    integer  npts,nmax,i,j,k,flag
    real*8  dx,norm
    real*8, allocatable :: r1(:),v1(:),r(:),pot(:),hamil_D(:),hamil_U(:),wf1(:),wf(:,:)
    REAL*8, ALLOCATABLE :: W(:),WORK(:),Z(:,:)
    INTEGER LWORK,INFO,LDZ

  ! Variables:
  ! ---------
  ! npts  = number of points
  ! dx    = stepsize
  ! nmax  = maximum principal number
  ! lmax  = maximum angular momentum

    print *,'input number of points and stepsize'
    read(5,*) npts,dx
    print *,'input maximum n'
    read(5,*) nmax

  ! The next two statements are for the LAPACK routine
    LWORK = 2*npts-2
    LDZ = npts

    allocate(r1(npts+1),v1(npts+1),r(npts+1),pot(npts+1),hamil_D(npts),hamil_U(npts-1),wf(npts+1,nmax),wf1(npts+1))
    allocate(W(npts+1), WORK(LWORK),Z(npts,npts))

    write(6,1000) npts,dx,npts*dx
    write(6,1001) nmax
    1000 format('npts = ',i6,'     dx = ',f10.6,'     rmax = ',f12.4)
    1001 format('nmax = ',i6)
    write(6,*)

  ! Set up the grid and set potential
    Do i=1,npts+1
      r(i)=(i-1)*dx
      pot(i) = 0.5*(r(i) - npts*dx/2)**2
      write(3,"(2f16.6)") r(i),pot(i)
    Enddo

  ! Start diagonalization of hamiltonian
    print*,'****************************************'
    print*,'Setting up and Diagonalizing Hamiltonian'
    print*,'****************************************'
    print*,'---------------'
    print*,'Energies (a.u.)'
    print*,'---------------'

    Z = 0.d0
    Do i=1,npts
      Z(i,i) = 1.0D0
    Enddo
    hamil_U = 0.d0
    hamil_D = 0.d0
    Do i=1,npts-1
      hamil_D(i) =  1.d0/dx**2
      hamil_D(i) = hamil_D(i) + pot(i+1)
      if (i < (npts-1)) hamil_U(i) = -0.5d0/dx**2
    Enddo
    CALL DSTEQR('I',npts-1,hamil_D,hamil_U,Z,LDZ,WORK,INFO)
    write(*,1003) (hamil_D(j),j=1,nmax+1)
    1003 format(1p10e14.6)
    write(*,*)
  ! Renormalize with Simpson rule 
    Do j=1,nmax+1
      wf1(:) = abs(Z(:,j))**2
      wf(:,j) = Z(:,j)
      CALL ARSIMD(npts,dx,wf1,norm)
      wf(:,j) = wf(:,j)/dsqrt(norm)
    Enddo

  ! Write output file with the states in order 
  ! [s-state from n= 1,...,nmax, then p-state from n= 1,...,nmax-1,...]
    open(9,file='wfn.out')
    write(9,999) r(1),pot(1),(wf(1,j),j=1,nmax+1)
    999 format(600ES16.6E3)
  ! Ensure that all wavefunctions start with positive values
    Do j=1,nmax+2
      flag = 0
      Do k=1,npts
        if (abs(wf(k,j)) < 1e-16 .OR. flag == 1) cycle
        flag = 1
        if(mod(j+1,2) == 0 .AND. wf(k,j) > 0.d0) cycle
        if (mod(j+1,2) == 1 .AND. wf(k,j) < 0.d0) cycle
        Do i=1,npts-1
          wf(i,j) = -wf(i,j)
        Enddo
        wf(npts,j) = 0.0d0
        exit
      Enddo
    Enddo

  !
    Do i=1,npts
      write(9,999) r(i+1),pot(i+1),(wf(i,j),j=1,nmax+1)
    Enddo

  End Program

!================================================================================================================================
  SUBROUTINE ARSIMD(N,DEL,A,R)
!================================================================================================================================
    IMPLICIT NONE 
    INTEGER N,L,I
    REAL*8 DEL,R,SUM,A(*)
    L = N
    SUM = A(1)-A(L)
    DO I=2,L,2
      SUM = SUM + 4.D0*A(I) + 2.D0*A(I+1)
    ENDDO
    R = (DEL*SUM)/3.D0
    RETURN
  END SUBROUTINE
