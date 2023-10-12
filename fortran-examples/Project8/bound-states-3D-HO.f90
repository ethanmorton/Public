!================================================================================================================================
  Program Diagonalization
!================================================================================================================================
    implicit none
    integer  npts,ntotstates,lmax,nmax,i,j,il,q
    real*8  dx,norm,lang,h,hmax,alpha_R,xmin,rmax
    real*8, allocatable, dimension(:) :: r,v,hamil_D,hamil_U,wf1,had,L
    REAL*8, ALLOCATABLE :: W(:),WORK(:),Z(:,:),wf(:,:,:)
    INTEGER LWORK,INFO,LDZ
    character*1024, allocatable :: state(:)

  ! Variables:
  ! ---------
  ! npts  = number of points
  ! dx    = stepsize
  ! nmax  = maximum principal number
  ! lmax  = maximum angular momentum

    print *,'input number of points and stepsize'
    read(5,*) rmax,dx
    print *,'input maximum n and l'
    read(5,*) nmax,lmax
    ntotstates = (lmax+1)*nmax - lmax*(lmax+1)/2

    h = 0.1d0*dx
    hmax = 5.d0*dx
    alpha_R = 2.d0
    xmin = 10.d0

    call calc_npts(npts,h,hmax,alpha_R,xmin,rmax,dx)

  ! The next two statements are for the LAPACK routine
    LWORK = 2*npts-2
    LDZ = npts

    allocate(r(npts+1),v(npts+1),hamil_D(npts),hamil_U(npts-1))
    allocate(wf(npts+1,nmax,0:lmax),wf1(npts+1),state(ntotstates),had(npts),L(npts))
    allocate(W(npts+1),WORK(LWORK),Z(npts,npts))

    write(6,1000) npts,rmax
    write(6,1001) nmax,lmax
    1000 format('npts = ',i6,'     rmax = ',f12.4)
    1001 format('nmax = ',i6,'     lmax = ',2i5)
    write(6,*)

    r(1) = 0.d0
    v(1) = 0.5*r(1)**2
    had(1) = h

  ! Set up the grid and set potential
    Do i=2,npts
      if (r(i-1) > xmin) then
        had(i) = had(i-1)*(1.d0 + alpha_R*had(i-1))
      else
        had(i) = h
      endif
      if (had(i) > hmax) had(i) = hmax
      if (i == npts) had(i) = dx
      L(i) = dsqrt((had(i) + had(i-1))/2.d0)
      r(i) = r(i-1) + had(i-1)
      v(i) = 0.5*r(i)**2
    Enddo

  ! Start diagonalization of hamiltonian
    print*,'****************************************'
    print*,'Setting up and Diagonalizing Hamiltonian'
    print*,'****************************************'
    print*,'---------------'
    print*,'Energies (a.u.)'
    print*,'---------------'
    Do il = 0,lmax
      Z = 0.d0
      Do i=1,npts
        Z(i,i) = 1.0D0
      Enddo
      lang = dble(il)
      hamil_U = 0.d0
      hamil_D = 0.d0
      Do i=1,npts-1
        hamil_D(i) =  1.d0/(had(i+1)*had(i))
        hamil_D(i) = hamil_D(i) + lang * (lang + 1.d0)/2.d0/r(i+1)**2 + v(i+1)
        if (i < (npts-1)) hamil_U(i) = -1.0d0/(had(i+1)*dsqrt(had(i+2) + had(i+1))*dsqrt(had(i) + had(i+1)))
        !hamil_U(i) = -0.5d0/dx**2
      Enddo
      CALL DSTEQR('I',npts-1,hamil_D,hamil_U,Z,LDZ,WORK,INFO)
      write(*,1002) il
      write(*,1003) (hamil_D(j),j=1,nmax-il)
      1002 format(/,'energies for l = ',i3,':',/)
      1003 format(1p10e14.6)
      write(*,*)
      Do j=1,nmax-il
        norm = 0.d0
        Do i=1,npts-1
          wf1(i) = abs(Z(i,j)/L(i+1))**2
          norm = norm + wf1(i)*L(i+1)**2
        Enddo
        Do i=1,npts-1
          wf(i,j,il) = Z(i,j)/L(i+1)/dsqrt(norm)
        Enddo
      Enddo
    Enddo
    
  !   Do il = 0,lmax
  !     Z = 0.d0
  !     Do i=1,npts
  !       Z(i,i) = 1.0D0
  !     Enddo
  !     lang = dble(il)
  !     hamil_U = 0.d0
  !     hamil_D = 0.d0
  !     Do i=1,npts-1
  !       hamil_D(i) =  1.d0/dx**2
  !       hamil_D(i) = hamil_D(i) + lang * (lang + 1.d0)/2.d0/r(i+1)**2 + v(i+1)
  !       if (i < (npts-1)) hamil_U(i) = -0.5d0/dx**2
  !     Enddo
  !     CALL DSTEQR('I',npts-1,hamil_D,hamil_U,Z,LDZ,WORK,INFO)
  !     write(*,1002) il
  !     write(*,1003) (hamil_D(j),j=1,nmax-il)
  !     1002 format(/,'energies for l = ',i3,':',/)
  !     1003 format(1p10e14.6)
  !     write(*,*)
  ! !   Renormalize with Simpson rule 
  !     Do j=1,nmax-il
  !       wf1(:) = abs(Z(:,j))**2
  !       wf(:,j,il) = Z(:,j)
  !       CALL ARSIMD(npts,dx,wf1,norm)
  !       wf(:,j,il) = wf(:,j,il)/dsqrt(norm)
  !     Enddo
  !   Enddo

  ! Write output file with the states in order 
  ! [s-state from n= 1,...,nmax, then p-state from n= 1,...,nmax-1,...]
    open(9,file='wfn.out')

    Do q=1,ntotstates
      write(state(q),"(i3.3,a)") q+2
    Enddo

    write(9,*) "#       1               2              ",(trim(state(q)),"             ",q=1,ntotstates)

    q = 0
    Do il=0,lmax
      Do j=il+1,nmax
        q = q + 1
        write(state(q),"(a,i2.2,a,i2.2)") " n = ",j-1," l = ",il
      Enddo
    Enddo

    write(9,*) "#    position       potential    ",(trim(state(q)),"  ",q=1,ntotstates)
    write(9,999) r(1),v(1),(0.d0,j=1,ntotstates)
    999 format(600ES16.6E3)
  ! Ensure that all wavefunctions start with positive values
    Do il=0,lmax
      Do j=1,nmax-il
        if (wf(2,j,il) <= 0.0d0) then
          Do i=1,npts-1
            wf(i,j,il) = -wf(i,j,il)
          Enddo
          wf(npts,j,il) = 0.0d0
        endif
      Enddo
    Enddo
  !
    Do i=1,npts
      write(9,999) r(i+1),v(i+1),((wf(i,j,il),j=1,nmax-il),il=0,lmax)
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

!================================================================================================================================
  subroutine calc_npts(npts,h,hmax,alpha_R,xmin,rmax,dx)
!================================================================================================================================
    implicit none
    integer npts
    real*8 h,hmax,alpha_R,xmin,r,rmax,had1,had2,dx

    npts = 0
    r = 0.d0
    had1 = h

    Do 
      if (r >= rmax) exit
      npts = npts + 1
      if (r > xmin) then
        had2 = had1*(1.d0 + alpha_R*had1)
      else
        had2 = h
      endif
      if (had2 > hmax) had2 = hmax
      r = r + had1
      dx = had1
      had1 = had2
    Enddo

    dx = rmax - (r - dx)

  end subroutine