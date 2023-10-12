program multiply
    implicit none
    integer, dimension(:,:), allocatable :: square_matrix
    integer, dimension(:,:), allocatable :: vector_matrix
    integer, dimension(:,:), allocatable :: output_matrix
    integer :: values(8)
    integer :: i, j, k, n, m, sum, start_time, end_time
    character :: outputs
    character(10) :: arg1, arg2

    !setup
    call get_command_argument(1, arg1)
    read (arg1,*) n
    allocate(square_matrix(1:n, 1:n))
    
    call get_command_argument(2, arg2)
    read (arg2,*) m
    allocate(vector_matrix(1:m, 1:n))
    allocate(output_matrix(1:m, 1:n))

    call set_start_time()

    call generate_matrices()

    ! actual calculations
    do i = 1, m
        do j = 1, n
            sum = 0
            do k = 1, n
                sum = sum + square_matrix(k, j) * vector_matrix(i, k)
            end do
            output_matrix(i, j) = sum
        end do
    end do

    call set_end_time()
    write (*,*) "Took ", end_time - start_time, "ms"

    ! printing stuff
    write (*,*) "Print results? (y/n)"
    read *, outputs
    if ( outputs == 'y' ) then
        write(*,*) "Square ::=> "
        call print_square()
        write(*,*) "Vector ::=> "
        call print_vector()
        write(*,*) "Output ::=> "
        call print_output()
    end if
    
    contains
        subroutine generate_matrices()
            do i=1,n
                do j=1,n
                    square_matrix(i,j) = i + j
                end do
            end do
            do i = 1, m
                do j = 1,n
                    vector_matrix(i,j) = i + j
                end do
            end do
        end subroutine generate_matrices

        subroutine print_square()
            do i = 1, n
                do j = 1, n
                    write(*, "(i10, 2x)", advance="no") square_matrix(i, j)
                end do
                write(*,*)
            end do
        end subroutine print_square

        subroutine print_vector()
            do i = 1, n
                write (*,*) ( vector_matrix(j, i), j=1,m )
            end do
        end subroutine print_vector

        subroutine print_output()
            do i = 1, n
                write (*,*) ( output_matrix(j, i), j=1,m )
            end do
        end subroutine print_output

        subroutine set_start_time()
            call date_and_time(values=values)
            start_time = values(5)*3600000 + values(6)*60000 + values(7)*1000 + values(8)
        end subroutine set_start_time

        subroutine set_end_time()
        call date_and_time(values=values)
            end_time = values(5)*3600000 + values(6)*60000 + values(7)*1000 + values(8)
        end subroutine set_end_time
end program multiply