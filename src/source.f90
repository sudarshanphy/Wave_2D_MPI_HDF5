subroutine get_source_position(nx_per_core, snx_rank, snx_local)
    ! this subroutine gets the rank where the source is present
    ! as well as the local index of the source
    use variable, only: sx, sy, snx, sny, dx, dy, nprocs
    
    implicit none
    integer, dimension(nprocs), intent(in) :: nx_per_core
    integer, intent(out) :: snx_rank, snx_local
    integer :: i
    snx = nint(sx/dx) + 1
    sny = nint(sy/dy) + 1

    if (snx <= nx_per_core(1)) then
         snx_local = snx + 1
         snx_rank = 0
    end if

    if (nprocs > 1) then
        do i = 1, nprocs - 1
             if ((snx > sum(nx_per_core(1:i))) .and. (snx <= sum(nx_per_core(1:i+1)))) then
                snx_local = snx - sum(nx_per_core(1:i)) + 1
                snx_rank = i
             end if
        end do
    end if

    !print *, 'snx_local: ', snx_local, 'snx_rank: ', snx_rank 
end subroutine

