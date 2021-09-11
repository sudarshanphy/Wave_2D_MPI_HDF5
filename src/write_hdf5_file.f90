module write_hdf5

contains 

subroutine get_filename(k)
    ! this subrotuine first converts an integer k to string
    ! and then returns a file name for hdf5 file.

    use variable, only: int_to_str, filename
    implicit none

    integer, intent(in) :: k
    
    ! convert k=12 to int_to_str='0012'
    write(int_to_str, '(i4.4)') k
    filename = './output_files/wave_2d_'//trim(adjustl(int_to_str))//'.h5'

end subroutine

subroutine write_hdf5_file(filename, solution)

    use hdf5 ! This module contains all necessary HDF5 modules
    use variable, only: runtime_parameter_values, t, x, y, &
                        runtime_parameter_names
    implicit none

    ! Names (file and HDF5 objects)
    character(len=200), intent(in) :: filename  ! File name
    !real*8, intent(in) :: time 
    double precision, dimension(:, :), intent(in) :: solution
    !real*8, dimension(7) :: runtime_val
    ! Dataset 1 name 
    character(len=20), parameter :: dsetname1 = "time(t)" 
    ! Dataset 2 name
    character(len=20), parameter :: dsetname2 = "solution(unp1)"
    ! Dataset 3 name
    character(len=20), parameter :: dsetname3 = "runtime_values"
    ! Dataset 4 name
    character(len=20), parameter :: dsetname4 = "runtime_names"
    ! Dataset 5 name
    character(len=20), parameter :: dsetname5 = "x_grid_points"
    ! Dataset 6 name
    character(len=20), parameter :: dsetname6 = "y_grid_points"
    
    ! Identifiers
    integer(hid_t) :: file_id       ! File identifier
    integer(hid_t) :: dset1_id      ! Dataset 1 identifier
    integer(hid_t) :: dset2_id      ! Dataset 2 identifier
    integer(hid_t) :: dset3_id      ! Dataset 3 identifier
    integer(hid_t) :: dset4_id      ! Dataset 4 identifier
    integer(hid_t) :: dset5_id      ! Dataset 5 identifier
    integer(hid_t) :: dset6_id      ! Dataset 6 identifier
    integer(hid_t) :: dspace1_id    ! Dataspace 1 identifier
    integer(hid_t) :: dspace2_id    ! Dataspace 2 identifier
    integer(hid_t) :: dspace3_id    ! Dataspace 3 identifier
    integer(hid_t) :: dspace4_id    ! Dataspace 4 identifier
    integer(hid_t) :: dspace5_id    ! Dataspace 5 identifier
    integer(hid_t) :: dspace6_id    ! Dataspace 6 identifier
  
    ! Integer array
    integer :: rank1                  ! Dataset rank
    integer(hsize_t), dimension(1) :: dims1  ! Dataset dimensions
    integer(hsize_t), dimension(1) :: data_dims1
    double precision, dimension(1) :: dset_data1   ! Data buffers

    integer :: rank2
    integer(hsize_t), dimension(2) :: dims2 
    integer(hsize_t), dimension(2) :: data_dims2
    double precision, dimension(size(solution,1), size(solution,2)) :: dset_data2
  
    integer :: rank3
    integer(hsize_t), dimension(1) :: dims3 
    integer(hsize_t), dimension(1) :: data_dims3
    double precision, dimension(size(runtime_parameter_values)) :: dset_data3
    
    integer :: rank4
    integer(hsize_t), dimension(1) :: dims4 
    integer(hsize_t), dimension(1) :: data_dims4
    character(len=20), dimension(size(runtime_parameter_names)) :: dset_data4
    
    integer :: rank5
    integer(hsize_t), dimension(1) :: dims5
    integer(hsize_t), dimension(1) :: data_dims5
    double precision, dimension(size(x)) :: dset_data5
    
    integer :: rank6
    integer(hsize_t), dimension(1) :: dims6
    integer(hsize_t), dimension(1) :: data_dims6
    double precision, dimension(size(y)) :: dset_data6
  
    ! Misc variables (e.g. loop counters)
    integer :: error ! Error flag
    integer :: i

    ! By default, H5T_NATIVE_CHARACTER only stores 1 byte.
    ! thus, to read charazters with more than 1 byte, we need
    ! to define new data type
    integer(size_t) :: size1 = 20  ! size of a new data type
    integer(hid_t) :: H5T_CHARACTER_20 ! new data type
! =====================================================================

    rank1 = 1
    rank2 = 2
    rank3 = 1
    rank4 = 1
    rank5 = 1
    rank6 = 1

    dims1 = (/1/)
    dims2 = (/size(solution,1), size(solution,2)/)
    dims3 = (/size(runtime_parameter_values)/)
    dims4 = (/size(runtime_parameter_names)/)
    dims5 = (/size(x)/)
    dims6 = (/size(y)/)
    
    ! Initialize Fortran interface
    CALL h5open_f(error)   
    ! Create a new file
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

    ! Create dataspace 1 (the dataset is next) "dspace_id" is returned
    CALL h5screate_simple_f(rank1, dims1, dspace1_id, error)
    ! Create dataset 1 with default properties "dset_id" is returned
    CALL h5dcreate_f(file_id, dsetname1, H5T_NATIVE_DOUBLE, dspace1_id, &
                     dset1_id, error)
    
    ! Initialize the dset_data array 
    data_dims1 = 1
    dset_data1(1) = t

    ! Write dataset 1
    CALL h5dwrite_f(dset1_id, H5T_NATIVE_DOUBLE, dset_data1, data_dims1, &
                    error)
    ! Close access to dataset 1
    CALL h5dclose_f(dset1_id, error)
    ! Close access to data space 1
    CALL h5sclose_f(dspace1_id, error)
  
    ! Create dataspace 2
    CALL h5screate_simple_f(rank2, dims2, dspace2_id, error)
    ! Create dataset 2 with default properties
    CALL h5dcreate_f(file_id, dsetname2, H5T_NATIVE_DOUBLE, dspace2_id, &
                     dset2_id, error)

    data_dims2 = (/size(solution,1), size(solution,2)/)
    dset_data2(:,:) = solution(:,:)

    ! Write dataset 2
    CALL h5dwrite_f(dset2_id, H5T_NATIVE_DOUBLE, dset_data2, data_dims2, &
                    error)
    ! Close access to dataset 2
    CALL h5dclose_f(dset2_id, error)
    ! Close access to data space 2
    CALL h5sclose_f(dspace2_id, error)
  
    ! Create dataspace 3
    CALL h5screate_simple_f(rank1, dims3, dspace3_id, error)
    ! Create dataset 3 with default properties
    CALL h5dcreate_f(file_id, dsetname3, H5T_NATIVE_DOUBLE, dspace3_id, &
                     dset3_id, error)

    data_dims3 = (/size(runtime_parameter_values)/)
    dset_data3(:) = runtime_parameter_values(:)
    
    ! Write dataset 3
    CALL h5dwrite_f(dset3_id, H5T_NATIVE_DOUBLE, dset_data3, data_dims3, &
                    error)
    ! Close access to dataset 3
    CALL h5dclose_f(dset3_id, error)
    ! Close access to data space 3
    CALL h5sclose_f(dspace3_id, error)
  
    ! Create dataspace 4
    CALL h5screate_simple_f(rank4, dims4, dspace4_id, error)

    ! create new data type which stores 20 byte data instead of 1
    call h5tcopy_f(H5T_NATIVE_CHARACTER, H5T_CHARACTER_20, i)
    call h5tset_size_f(H5T_CHARACTER_20, size1, i)
    
    ! Create dataset 4 with default properties
    CALL h5dcreate_f(file_id, dsetname4, H5T_CHARACTER_20, dspace4_id, &
                     dset4_id, error)

    data_dims4 = (/size(runtime_parameter_names)/)
    dset_data4(:) = runtime_parameter_names(:)

    ! Write dataset 4
    CALL h5dwrite_f(dset4_id, H5T_CHARACTER_20, dset_data4, data_dims4, &
                    error)
    ! Close access to dataset 4
    CALL h5dclose_f(dset4_id, error)
    ! Close access to data space 4
    CALL h5sclose_f(dspace4_id, error)
  
    ! Create dataspace 5
    CALL h5screate_simple_f(rank5, dims5, dspace5_id, error)
    ! Create dataset 3 with default properties
    CALL h5dcreate_f(file_id, dsetname5, H5T_NATIVE_DOUBLE, dspace5_id, &
                     dset5_id, error)

    data_dims5 = (/size(x)/)
    dset_data5(:) = x(:)
    
    ! Write dataset 5
    CALL h5dwrite_f(dset5_id, H5T_NATIVE_DOUBLE, dset_data5, data_dims5, &
                    error)
    ! Close access to dataset 5
    CALL h5dclose_f(dset5_id, error)
    ! Close access to data space 5
    CALL h5sclose_f(dspace5_id, error)
  
    ! Create dataspace 6
    CALL h5screate_simple_f(rank6, dims6, dspace6_id, error)
    ! Create dataset 3 with default properties
    CALL h5dcreate_f(file_id, dsetname6, H5T_NATIVE_DOUBLE, dspace6_id, &
                     dset6_id, error)

    data_dims6 = (/size(y)/)
    dset_data6(:) = y(:)
    
    ! Write dataset 3
    CALL h5dwrite_f(dset6_id, H5T_NATIVE_DOUBLE, dset_data6, data_dims6, &
                    error)
    ! Close access to dataset 3
    CALL h5dclose_f(dset6_id, error)
    ! Close access to data space 3
    CALL h5sclose_f(dspace6_id, error)
  
   ! Close the file
   CALL h5fclose_f(file_id, error)
   ! Close FORTRAN interface
   CALL h5close_f(error)

end subroutine

end module write_hdf5
