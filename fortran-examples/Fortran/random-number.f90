program random_num
    implicit none
    real :: random(1)
    call random_number(random)
    print "(1f6.5)", random
end program random_num