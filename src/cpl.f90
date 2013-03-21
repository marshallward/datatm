module cpl_mod
    use mod_oasis
    use mpp_mod     ! Includes MPI header
    implicit none

    integer :: oasis_comp_id
    character (len=6), parameter :: oasis_comp_name = 'datatm'

contains

    !------------------
    subroutine cpl_init
        ! Initialise the global MPI communicator and the OASIS coupler
        ! Return the local MPI communicator ID

        character(len=*), parameter :: method_name = 'coupler_init'
        integer :: ierr

        ! Pre-initialise MPI until that vayu bug is sorted out
        call MPI_Init(ierr)
        if (ierr /= MPI_SUCCESS) stop(-1)

        call oasis_init_comp(oasis_comp_id, oasis_comp_name, ierr)
        if (ierr /= OASIS_OK) then
            call oasis_abort(oasis_comp_id, method_name, 'STOP 1')
        endif

        call oasis_get_localcomm(mpp_comm_id)
        if (ierr /= OASIS_OK) then
            call oasis_abort(oasis_comp_id, method_name, 'STOP 2')
        endif

    end subroutine cpl_init


    !---------------------------------------------------------
    function cpl_partition_init(p_type, p_length, p_width) result(partition_id)
        ! TODO: add optional arguments for other partition types

        integer :: partition_id
        character(len=*), intent(in) :: p_type
        integer, intent(in) :: p_length
        integer, intent(in), optional :: p_width

        select case(p_type)
            case ('serial')
                partition_id = cpl_partition_serial_init(p_length)

            case ('apple')
                print *, 'Apple is unimplemented'
                stop(-1)

            case ('box')
                partition_id = cpl_partition_box_init(p_length, p_width)

            case ('orange')
                print *, 'Orange is unimplemented'
                stop(-1)

            case default
                print *, 'Unknown partition type'
                stop(-1)
        end select

    end function cpl_partition_init


    !----------------------------------------------------------------
    function cpl_partition_serial_init(p_length) result(partition_id)

        integer :: partition_id
        integer, intent(in) :: p_length

        integer, dimension(3) :: p_config
        integer :: ierr

        p_config(1) = 0         ! serial
        p_config(2) = 0         ! Unused
        p_config(3) = p_length  ! Partition length

        call oasis_def_partition(partition_id, p_config, ierr)
        if (ierr /= OASIS_Ok) then
            call oasis_abort(oasis_comp_id, 'cpl_partition_serial_init', &
                             'serial init oops')
        end if

    end function cpl_partition_serial_init


    !---------------------------------------------------------------
    function cpl_partition_box_init(p_nx, p_ny) result(partition_id)
        ! TODO: This is a temporary box partition of the entire field

        integer :: partition_id
        integer, intent(in) :: p_nx
        integer, intent(in) :: p_ny

        integer, dimension(5) :: p_config
        integer :: ierr

        p_config(1) = 2         ! box
        p_config(2) = 0         ! top-left global offset
        p_config(3) = p_nx      ! local nx (here global)
        p_config(4) = p_ny      ! local ny (here global)
        p_config(5) = p_nx      ! global nx

        call oasis_def_partition(partition_id, p_config, ierr)
        if (ierr /= OASIS_Ok) then
            call oasis_abort(oasis_comp_id, 'cpl_partition_box_init', &
                             'box init oops')
        end if

    end function cpl_partition_box_init


    !---------------------------------------------------------------------
    subroutine cpl_var_init(var_name, partition_id, var_shape, cpl_var_id)
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: partition_id
        integer, dimension(:), intent(in) :: var_shape
        integer, intent(out) :: cpl_var_id

        integer :: rank
        integer, dimension(2) :: var_dims
        integer, dimension(:), allocatable :: var_index_range

        integer :: i
        integer :: ierr

        ! Currently these are parameters, but someday could be input arguments
        integer, parameter :: var_tx_type = OASIS_Out
        integer, parameter :: var_dtype = OASIS_Real

        ! Get variable shape and size
        ! TODO: assert var_shape is a 1D vector
        rank = size(var_shape)
        var_dims(1) = rank
        var_dims(2) = 1

        allocate(var_index_range(2*rank))
        do i = 1, rank
            var_index_range(2*i - 1) = 1
            var_index_range(2*i)     = var_shape(i)
        end do

        ! Register as OASIS variable
        call oasis_def_var(cpl_var_id, var_name, partition_id, var_dims, &
                           var_tx_type, var_shape, var_dtype, ierr)

    end subroutine cpl_var_init


    !---------------------------
    subroutine cpl_init_finalize

        integer :: ierr
        character(len=*), parameter :: method_name = 'cpl_init_finalize'

        call oasis_enddef(ierr)
        if (ierr /= OASIS_Ok) then
            call oasis_abort(oasis_comp_id, method_name, 'enddef oops')
        end if

    end subroutine cpl_init_finalize


    !-------------------------------
    subroutine cpl_push_field(cpl_id, field_val, t)

        integer, intent(in) :: cpl_id
        real, dimension(:,:), intent(in) :: field_val
        integer, intent(in) :: t

        integer :: ierr

        call oasis_put(cpl_id, t, field_val, ierr)
        if (ierr /= OASIS_Ok) then
            call oasis_abort(oasis_comp_id, 'cpl_send_field', 'put oops')
        end if

    end subroutine cpl_push_field


    !------------------
    subroutine cpl_exit

        character (len=*), parameter :: method_name = 'coupler_exit'
        integer :: ierr

        call oasis_terminate(ierr)
        if (ierr /= OASIS_OK) then
            call oasis_abort(oasis_comp_id, method_name, 'STOP 1')
        endif

        ! Manually terminate MPI until that vayu bug is sorted out
        call MPI_Finalize(ierr)
        if (ierr /= MPI_SUCCESS) stop(-1)
    end subroutine cpl_exit

end module cpl_mod
