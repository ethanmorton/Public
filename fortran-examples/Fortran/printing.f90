program printing
    implicit none
    print *, "An output ", 10
    !for integers, RiW => print "(RiW)" R = number of times W = space
    print "(3i5)",7,6,8          ! => 7     6      8
    print "(i5)",7,6,8           ! => 7
                                 !    6
                                 !    8

    !for floats, RfW.D => print "(RfW.D)" R = number of times per line W = number of characters D = precision
    print "(2f8.5)", 3.1415, 1.2345678       ! => 3.14150 1.23456

    !for newlines in strings, add / to "()" => print "(/, RaW)"
    print "(/, 2a8)", "Name", "Age"

    !for scientific notation, "(e10.D)" D = the exponent
    print "(e10.3)", 123.456    ! => 0.123E+03

    !you can combine these formats in however you want
    print "(a5,i2)", "I am ", 43       ! => I am 43
end program printing