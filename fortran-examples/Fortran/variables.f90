program variables
    implicit none
    !real means its a real number, parameter means its a constant
    real, parameter :: PI = 3.1415
    real :: r_num1 = 0.0, r_num2 = 0.0
    !a real provides 6 digits of precision, a double has 15 but requires this syntax
    double precision :: dbl_num = 1.11111111111111d+0
    integer :: i_num1 = 0, i_num2 = 0
    logical :: can_vote = .true., can_register = .false.
    character (len = 10) :: month
    complex :: com_num = (2.0, 40)

    print *, "Biggest Real ", huge(r_num1)
    print *, "Biggest Int ", huge(i_num1)
    print *, "Smallest Real ", tiny(r_num1)
    print "(a4, i1)", "Int ", kind(i_num1)
    print "(a5, i1)", "Real ", kind(r_num1)
    print "(a8, i1)", "Complex ", kind(com_num)
    print "(a8, i1)", "Logical ", kind(can_vote)

end program variables