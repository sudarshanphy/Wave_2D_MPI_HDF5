subroutine get_param()
         
    ! this subroutine reads from the parameter file and 
    ! passes the values as output
    use variable, only: Lx, Ly, sx, sy, dx, dy, T_final,&
                        C, cfl, runtime_parameter_values,&
                        runtime_parameter_names, &
                        xl_boundary, xr_boundary, yl_boundary, &
                        yr_boundary, filefrequency
    implicit none
    
    ! local varaibles to read the parameter file.
    character(len=100) :: buffer, label
    integer :: pos
    integer, parameter :: funit = 15 ! file unit number
    integer :: ios = 0
    integer :: line = 0 ! tracks number of lines in the paramter file
    
    ! set some default values, incase the user doesn't provide
    Lx = 10.0
    Ly = 10.0
    dx = 0.5
    dy = 0.5
    T_final = 5.0
    C = 1
    cfl = 0.5
    sx = Lx / 2.0
    sy = Ly / 2.0
    filefrequency = 1
    xl_boundary = 1  ! reflect 
    xr_boundary = 1  ! reflect
    yl_boundary = 1  ! reflect
    yr_boundary = 1  ! reflect
    
    ! If a runtime parameter is not defined in .par files
    runtime_parameter_values(:) = -1
    runtime_parameter_names(:) = 'unknown'
 
    open(funit, file='./par_input.par')
    
    ! ios is negative if an end of record condition is encountered or if
    ! an endfile condition was detected.  It is positive if an error was
    ! detected.  ios is zero otherwise.
    
    do while (ios == 0)
       read(funit, '(A)', iostat=ios) buffer ! read the line
       if (ios == 0) then
          line = line + 1 ! count the line
    
          ! Find the first instance of whitespace.  Split label and data.
          pos = scan(buffer, '=')
          label = buffer(1:pos-2) ! this makes sure that we just read the label
          buffer = buffer(pos+1:) ! this is to read the value of the label
    
          select case (label) ! based on the label, now assign values to predefined varaibles
          case ('Lx')
             read(buffer, *, iostat=ios) Lx
             runtime_parameter_values(1) = Lx
             runtime_parameter_names(1) = 'Lx'
         case ('Ly')
             read(buffer, *, iostat=ios) Ly
             runtime_parameter_values(2) = Ly
             runtime_parameter_names(2) = 'Ly'
          case ('dx')
             read(buffer, *, iostat=ios) dx
             runtime_parameter_values(3) = dx
             runtime_parameter_names(3) = 'dx'
          case ('dy')
             read(buffer, *, iostat=ios) dy
             runtime_parameter_values(4) = dy
             runtime_parameter_names(4) = 'dy'
          case ('tfinal')
             read(buffer, *, iostat=ios) T_final
             runtime_parameter_values(5) = T_final
             runtime_parameter_names(5) = 'tfinal'
          case ('C')
             read(buffer, *, iostat=ios) C
             runtime_parameter_values(6) = C
             runtime_parameter_names(6) = 'C'
          case ('cfl')
             read(buffer, *, iostat=ios) cfl
             runtime_parameter_values(7) = cfl
             runtime_parameter_names(7) = 'cfl'
          case ('sx')
             read(buffer, *, iostat=ios) sx
             runtime_parameter_values(8) = sx
             runtime_parameter_names(8) = 'sx'
          case ('sy')
             read(buffer, *, iostat=ios) sy
             runtime_parameter_values(9) = sy
             runtime_parameter_names(9) = 'sy'
          case ('xl_boundary')
             read(buffer, *, iostat=ios) xl_boundary
             runtime_parameter_values(10) = xl_boundary
             runtime_parameter_names(10) = 'xl_boundary'
         case ('xr_boundary')
             read(buffer, *, iostat=ios) xr_boundary
             runtime_parameter_values(11) = xr_boundary
             runtime_parameter_names(11) = 'xr_boundary'
          case ('yl_boundary')
             read(buffer, *, iostat=ios) yl_boundary
             runtime_parameter_values(12) = yl_boundary
             runtime_parameter_names(12) = 'yl_boundary'
          case ('yr_boundary')
             read(buffer, *, iostat=ios) yr_boundary
             runtime_parameter_values(13) = yr_boundary
             runtime_parameter_names(13) = 'yr_boundary'
          case ('filefrequency')
             read(buffer, *, iostat=ios) filefrequency
             runtime_parameter_values(14) = filefrequency
             runtime_parameter_names(14) = 'filefrequency'
          case default
            ! for comments on the paramter file                
          end select
       end if
    end do
    close(funit) !close the file
end subroutine
