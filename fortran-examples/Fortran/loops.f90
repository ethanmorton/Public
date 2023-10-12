program looping
    implicit none
    integer :: n, num
    real :: out = 1

    do n = 1, 10
        print *, n
    end do
    print *, "Enter an integer to take the factorial"
    read *, num
    do n = 1, num
        out = out * n
    end do
    print*, "The factorial is ", out
end program looping