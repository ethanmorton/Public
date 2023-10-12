program youtube
    implicit none
    !character*20 :: name this is an alternate declaration way
    character (len=20) :: f_name, l_name

    print*, "Whats your name? "
    read *, f_name, l_name
    print *, "Hello ", trim(f_name), " ", trim(l_name)
    
end program youtube