module io_mod
    ! TODO: Maybe just load all netCDF IDs at the start

    use netcdf
    implicit none

    type io_data

        integer :: file_id

        integer :: var_id
        integer :: n_lon, n_lat

        integer :: time_id
        integer :: n_time

    end type io_data

contains

    !----------------------------------------
    subroutine io_open(filename, varname, io)

        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: varname
        type(io_data), intent(out) :: io

        integer :: ierr
        integer, dimension(:), allocatable :: field_shape

        ierr = nf90_open(filename, NF90_NOWRITE, io%file_id)
        if (ierr /= NF90_NOERR) stop(-1)

        call io_detect_time_var(io%file_id, io%time_id, io%n_time)

        ierr = nf90_inq_varid(io%file_id, varname, io%var_id)
        if (ierr /= NF90_NOERR) stop(-1)

        call io_get_field_shape(io, field_shape)
        io%n_lon = field_shape(1)
        io%n_lat = field_shape(2)

    end subroutine io_open


    !------------------------------------------------------
    subroutine io_detect_time_var(file_id, time_id, n_time)

        integer, intent(in) :: file_id
        integer, intent(out) :: time_id
        integer, intent(out) :: n_time

        integer :: file_nvars
        integer, dimension(:), allocatable :: file_var_ids

        integer :: t_dim_id

        integer :: tmp_ndims
        integer, dimension(:), allocatable :: tmp_dim_ids

        integer :: ierr
        integer :: i

        ! 1. Determine the number of variables
        ierr = nf90_inquire(file_id, nVariables=file_nvars, &
                            unlimitedDimId=t_dim_id)
        if (ierr /= NF90_NOERR) stop(-1)

        allocate(file_var_ids(file_nvars))

        ! 2. Get the variable IDs
        ierr = nf90_inq_varids(file_id, file_nvars, file_var_ids)
        if (ierr /= NF90_NOERR) stop(-1)

        ! 3. Identify a vector using the unlimited dimension axis
        do i = 1, file_nvars

            ierr = nf90_inquire_variable(file_id, file_var_ids(i), &
                                         ndims=tmp_ndims)
            if (ierr /= NF90_NOERR) stop(-1)

            if (tmp_ndims == 1) then
                ! nf90_inquire_variable expects a vector, even if rank 1
                allocate(tmp_dim_ids(tmp_ndims))

                ierr = nf90_inquire_variable(file_id, file_var_ids(i), &
                                             dimids=tmp_dim_ids)
                if (ierr /= NF90_NOERR) stop(-1)

                if (tmp_dim_ids(1) == t_dim_id) then
                    time_id = file_var_ids(i)
                    exit
                end if

                deallocate(tmp_dim_ids)
            end if

            ! TODO: Abort if no time axis can be identified
        end do

        ! Get length of time axis
        ierr = nf90_inquire_dimension(file_id, t_dim_id, len=n_time)
        if (ierr /= NF90_NOERR) stop(-1)

    end subroutine io_detect_time_var


    !-----------------------------------------
    subroutine io_allocate_time_axis(io, time)
        ! TODO: Merge unlimited dim assumption with io_detect_time_axis

        type(io_data), intent(in) :: io
        real, dimension(:), allocatable, intent(out) :: time

        integer :: ierr
        integer :: t_dim_id
        integer :: nt

        ierr = nf90_inquire(io%file_id, unlimitedDimId=t_dim_id)
        if (ierr /= NF90_NOERR) stop(-1)

        ierr = nf90_inquire_dimension(io%file_id, t_dim_id, len=nt)
        if (ierr /= NF90_NOERR) stop(-1)

        allocate(time(nt))

        ierr = nf90_get_var(io%file_id, io%time_id, time)
        if (ierr /= NF90_NOERR) stop(-1)

    end subroutine io_allocate_time_axis


    !---------------------------------------------
    subroutine io_get_field_shape(io, field_shape)

        type(io_data), intent(in) :: io
        integer, dimension(:), allocatable, intent(out) :: field_shape

        integer :: t_dim_id

        integer :: var_ndims
        integer, dimension(:), allocatable :: var_dim_ids

        integer :: ierr
        integer :: i, j
        integer :: n_dim

        ! NOTE: Second time we are fetching this
        ierr = nf90_inquire(io%file_id, unlimitedDimId=t_dim_id)
        if (ierr /= NF90_NOERR) stop(-1)

        ierr = nf90_inquire_variable(io%file_id, io%var_id, ndims=var_ndims)
        if (ierr /= NF90_NOERR) stop(-1)

        allocate(var_dim_ids(var_ndims))
        ierr = nf90_inquire_variable(io%file_id, io%var_id, dimids=var_dim_ids)
        if (ierr /= NF90_NOERR) stop(-1)

        allocate(field_shape(var_ndims - 1))
        j = 1
        do i = 1, var_ndims
            if (var_dim_ids(i) /= t_dim_id) then
                ierr = nf90_inquire_dimension(io%file_id, var_dim_ids(i), &
                                              len=field_shape(j))
                if (ierr /= NF90_NOERR) stop(-1)
                j = j + 1
            end if
        end do

    end subroutine io_get_field_shape


    !------------------------------------
    subroutine io_allocate_field(io, val)
        ! NOTE: Only necessariy if io is private

        type(io_data), intent(in) :: io
        real, dimension(:,:), allocatable, intent(inout) :: val

        allocate(val(io%n_lon, io%n_lat))

    end subroutine io_allocate_field

end module io_mod
