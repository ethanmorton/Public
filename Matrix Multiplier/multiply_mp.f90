program multiply
    use omp_lib
    implicit none
    integer, dimension(:,:), allocatable :: square_matrix
    integer, dimension(:,:), allocatable :: vector_matrix
    integer, dimension(:,:), allocatable :: output_matrix
    integer :: i, j, k, n, m, sum
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

    call generate_matrices()

    ! actual calculations
    !$omp parallel shared(square_matrix, vector_matrix, output_matrix, n, m) private(sum, i, j, k)
        !$omp do
        do i = 1, m
            do j = 1, n
                sum = 0
                do k = 1, n
                    sum = sum + square_matrix(j, k) * vector_matrix(i, k)
                end do
                output_matrix(i, j) = sum
            end do
        end do
        !$omp end do
    !$omp end parallel

    ! printing stuff
    write (*,*) "Print results?"
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
end program multiply