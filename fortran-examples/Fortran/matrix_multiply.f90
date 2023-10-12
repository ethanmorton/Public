program matrix_multiply
    implicit none
    real, dimension(0:15) :: a
    real, dimension(0:3) :: b, c
    integer :: n, w, ai, bi

    w = size(b)
    ai = 8
    bi = 17
    a = (/ (n, n = ai, ai + w**2 - 1 ) /)
    b = (/ (n, n = bi, bi + w -1     ) /)

    do n = 0, w**2 - 1
        c(floor(n * 1.0 / w)) = c(floor(n * 1.0 / w)) + a(n) * b(mod(n, w))
    end do

    print *, "a = "
    do n = 0, w**2 - 1
        print *, a(n)
    end do

    print *, "b = "
    do n = 0, w - 1
        print *, b(n)
    end do

    print *, "c = "
    do n = 0, w - 1
        print *, c(n)
    end do
end program matrix_multiply