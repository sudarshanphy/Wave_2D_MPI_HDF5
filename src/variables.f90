module variable

    implicit none
    ! store the grid points in x and y direction.
    double precision, allocatable, dimension(:), save :: x , y
    ! store the number of grid points in x per core.
    integer, allocatable, dimension(:), save :: nx_per_core
    ! store the 2D global arrays: solution at tn+1 which can be plotted
    double precision, allocatable, dimension(:, :), save ::  unp1_global
    ! some variables used in initialization
    double precision, save :: delta, t, dt
    ! usually i, j, k are used in do loops
    integer :: i, j, k
    ! variables to store the number of grid points
    integer, save ::  nx, ny
    ! variables to store the grid point of the source
    integer, save ::  snx, sny 
    ! variables to determine the rank and local index of source
    !integer, save ::  snx_local, snx_rank
    ! varibale that are read in through the paramerter file
    double precision, save :: Lx, Ly, sx, sy, dx, dy, T_final, C, cfl
    ! array to store the values from parameter file
    double precision, dimension(14), save :: runtime_parameter_values
    ! array to store names of the runtime parameters
    character(len=15), dimension(14), save :: runtime_parameter_names
    ! variables to store the filename to be written
    character(len=200), save :: filename, int_to_str
    ! variable to store boundary conditions
    integer, save :: xl_boundary, xr_boundary, yl_boundary, yr_boundary
    ! variable to store the frequency of output files
    integer, save :: filefrequency, fcount

    ! some variables for mpi calculation
    integer, save :: remainder, quotient, nprocs, ierr, comm
end module variable

module const

        implicit none

        double precision, parameter :: pi = 4.0 * atan(1.0)
        
end module const
