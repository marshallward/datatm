! Forcing fields
! Use the module to load and access the model forcing fields

! I really don't know how this ought to be set up

module field_mod
    use netcdf
    implicit none
    
    type ncvar
        integer :: file_id      ! Netcdf file ID
        integer :: var_id       ! Netcdf variable ID
        
        real, dimension(:), allocatable :: t        ! Time axis
        integer :: nt
        
        real, dimension(:, :), allocatable :: val   ! Field snapshot
        integer :: nx, ny
    end type ncvar

contains
    
    subroutine field_init(fname, vname, nc)
        ! TODO: Error handling
        !       Absence of unlimited dimension (t_dim_id == -1)
        
        character(len=*), intent(in) :: fname
        character(len=*), intent(in) :: vname
        type(ncvar), intent(out) :: nc
        
        integer :: t_dim_id
        integer :: t_id
        
        integer :: nc_nvars
        integer, dimension(:), allocatable :: nc_var_ids
        
        integer :: var_ndims
        integer, dimension(:), allocatable :: var_dim_ids
        integer, dimension(:), allocatable :: var_shape
        
        integer, dimension(2) :: new_var_dim_ids
        
        integer :: ierr
        integer :: i, j
        integer :: test_ndims
        integer, dimension(:), allocatable :: test_dim_ids
        
        ierr = nf90_open(fname, NF90_NOWRITE, nc%file_id)
        if (ierr /= NF90_NOERR) stop(-1)
        
        !----------
        ! Identify the unlimited (time) variable
        
        ierr = nf90_inquire(nc%file_id, nVariables=nc_nvars, &
                            unlimitedDimId=t_dim_id)
        if (ierr /= NF90_NOERR) stop(-1)
        
        allocate(nc_var_ids(nc_nvars))
        
        ierr = nf90_inq_varids(nc%file_id, nc_nvars, nc_var_ids)
        if (ierr /= NF90_NOERR) stop(-1)
        
        do i = 1, nc_nvars
            ierr = nf90_inquire_variable(nc%file_id, nc_var_ids(i), &
                                         ndims=test_ndims)
            if (ierr /= NF90_NOERR) stop(-1)
            
            if (test_ndims == 1) then
                ! nf90_inquire_variable expects a vector
                allocate(test_dim_ids(test_ndims))
                ierr = nf90_inquire_variable(nc%file_id, nc_var_ids(i), &
                                             dimids=test_dim_ids)
                if (test_dim_ids(1) == t_dim_id) then
                    t_id = nc_var_ids(i)
                    exit
                end if
                deallocate(test_dim_ids)
            end if
        end do
        
        deallocate(nc_var_ids)
        
        !-------
        ! Allocate and fill the time axis
        ! TODO: Maybe this isn't necessary
        
        ierr = nf90_inquire_dimension(nc%file_id, t_dim_id, len=nc%nt)
        if (ierr /= NF90_NOERR) stop(-1)
        
        allocate(nc%t(nc%nt))
        
        ierr = nf90_get_var(nc%file_id, t_id, nc%t)
        if (ierr /= NF90_NOERR) stop(-1)
        
        !-------
        ! Determine field size and allocate one snapshot
        
        ierr = nf90_inq_varid(nc%file_id, vname, nc%var_id)
        if (ierr /= NF90_NOERR) stop(-1)
        
        ierr = nf90_inquire_variable(nc%file_id, nc%var_id, ndims=var_ndims)
        if (ierr /= NF90_NOERR) stop(-1)
        
        allocate(var_dim_ids(var_ndims))
        ierr = nf90_inquire_variable(nc%file_id, nc%var_id, dimids=var_dim_ids)
        if (ierr /= NF90_NOERR) stop(-1)
        
        ! TODO: Generalise to work for any rank (currently assumes rank 2) 
        j = 1
        do i = 1, var_ndims
            if (var_dim_ids(i) /= t_dim_id) then
                new_var_dim_ids(j) = var_dim_ids(i)
                j = j+1
            end if
        end do
        deallocate(var_dim_ids)
        
        ierr = nf90_inquire_dimension(nc%file_id, new_var_dim_ids(1), &
                                      len=nc%nx)
        if (ierr /= NF90_NOERR) stop(-1)
        
        ierr = nf90_inquire_dimension(nc%file_id, new_var_dim_ids(2), &
                                      len=nc%ny)
        if (ierr /= NF90_NOERR) stop(-1)
        
        allocate(nc%val(nc%nx, nc%ny))
    
    end subroutine field_init
    
   
    subroutine field_update(nc, t)
        type(ncvar), intent(inout) :: nc
        real, intent(in) :: t
        
        integer :: i_t
        integer :: ierr
        
        ! Find the nearest index
        i_t = minloc(abs(nc%t - t), 1)
        
        ierr = nf90_get_var(nc%file_id, nc%var_id, nc%val, &
                            start=[ 1, 1, i_t], count=[ nc%nx, nc%ny, 1])
    
    end subroutine field_update


    subroutine field_terminate(nc)
        type(ncvar), intent(inout) :: nc
        integer :: ierr
        
        deallocate(nc%t)
        deallocate(nc%val)
        
        ierr = nf90_close(nc%file_id)
        if (ierr /= NF90_NOERR) stop(-1)
    
    end subroutine field_terminate

end module field_mod
