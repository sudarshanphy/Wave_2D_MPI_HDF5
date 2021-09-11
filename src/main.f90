! This is the main program for 2D wave equation with a source term
! Sudarshan Neopane, June 2021

program main
    ! varaible has all the variables used in this run
    ! const has the constant value, such as pi
    ! check varaiables.f90
    use variable
    use const
    use initialize
    use write_hdf5 
    use boundary_condition      
    use mpi 
    implicit none
    ! variables to store grid range per core
    integer :: a, b
    ! variables to store the local values of grid, source 
    ! and processor rank for source
    integer :: nx_local, snx_rank, snx_local   
    double precision, allocatable, dimension(:,:) :: un_local, unm1_local, unp1_local, sn_local 
    ! some mpi required integers
    integer :: my_rank, source
    integer, dimension(MPI_STATUS_SIZE) :: status1 
    comm = MPI_COMM_WORLD ! used frequently
    ! initialize mpi
    call MPI_INIT(ierr)
    
    ! get number of processors
    call MPI_COMM_SIZE(comm, nprocs, ierr)
    
    ! get processor rank
    call MPI_COMM_RANK(comm, my_rank, ierr)
    
    ! get the user defined parameter from paramter file
    ! check get_param.f90
    call get_param()
    
    ! calculate nx and ny; need to add 1 to make sure 
    ! that we get to the Lx and Ly values
    nx = int(Lx/dx) + 1 
    ny = int(Ly/dy) + 1
    
    ! allocate an array to store the files in each processor
    allocate(nx_per_core(nprocs))
    
    ! determine the number of grid points in x per processor
    ! #########################################################
    remainder = mod(nx, nprocs)
    quotient = (nx - remainder)/ nprocs

    do i = 1, nprocs
        nx_per_core(i) = quotient
    end do
    if (remainder >= 1) then
        do i = 1, remainder
            nx_per_core(i) = nx_per_core(i) + 1
        end do
    end if
    
    a = sum(nx_per_core(1:my_rank + 1)) - nx_per_core(my_rank + 1) + 1
    b = a + nx_per_core(my_rank + 1) - 1
    ! #########################################################

    ! not needed by other processors
    if (my_rank == 0) then 
        ! initilaize the grid
        ! check initialize.f90
        ! x = (/ 0, dx, ..., Lx/)
        ! y = (/ 0, dy, ..., Ly/)
        call init_grid(x, nx, Lx)
        call init_grid(y, ny, Ly)

        ! allocate array for global solution
        allocate(unp1_global(nx, ny))
        
        fcount = 0  ! file number counter
        k = 0  ! output file number
 
    end if
    
    ! get source grid position: snx, sny
    call get_source_position(nx_per_core, snx_rank, snx_local)
   

    nx_local = int(nx_per_core(my_rank + 1) + 2) ! 2 are ghost cells

    allocate(un_local(nx_local, ny))
    allocate(unm1_local(nx_local, ny))
    allocate(unp1_local(nx_local, ny))
    allocate(sn_local(nx_local, ny))
    ! initialize the 2D arrays to 0
    ! check initilaize.f90 
    ! un_local is the solution at time t(n)
    ! unm1_local is the solution at time t(n-1)
    ! unp1_local is the solution at time t(n+1)
    ! sn_local is the source at time t(n)
    call zeros(nx_local, un_local, unm1_local, unp1_local, sn_local)

    ! initilaize t = 0
    t = 0.0
    
    ! calculate dt; here dx=dy
    dt = cfl * dx / C
     
    ! loop until we reach the final time  
    do while (t < T_final)
            
        ! update time
        t = t + dt
        
        call apply_boundary(my_rank, nx_local, un_local, unp1_local)
        
        ! communicate the ghost cell values with different processors
        ! this subroutine also updates the local values 
        ! set u(t(n-1)) = u(t(n))
        ! set u(t(n)) = u(t(n+1))
        call communicate_all(my_rank, nx_local, un_local, unm1_local, &
                             unp1_local, sn_local)
 
        ! source term
        if (my_rank == snx_rank) then
            sn_local(snx_local,sny) = 20 * sin(30 * pi * t / 20)
        end if
        
        ! call the fdm solver to solve for unp1 at time t+dt
        ! check fdm_solver.f90
        call fdm_solver(dt, my_rank, nx_local , un_local, &
                        unm1_local, unp1_local, sn_local)
        
        ! collect the data in rank 0 proecssor 
        if (my_rank == 0) then
            call gather_at_zero(nx_local, unp1_local)
         else
            call send_to_zero(nx_local, unp1_local)
         end if
        
         ! write the output file
         if (my_rank == 0) then
 
            fcount = fcount + 1
            print *, 'calculation done for time: ', t 
            ! check is output file must be written or not
            if (mod(fcount, filefrequency) == 0) then

                ! get the filename based on value of k
                ! for k=12, filename: wave_2d_0012.h5
                ! check write_hdf5_file.f90
                call get_filename(k)

                ! write an hdf5 file
                ! check write_hdf_file.f90
                call write_hdf5_file(filename, unp1_global)

                k = k + 1
            end if
        end if
    end do

    if (my_rank == 0) deallocate(unp1_global)

    ! finalize mpi
    call MPI_FINALIZE(ierr)

    stop
end program main

