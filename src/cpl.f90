module cpl_mod
    use mod_oasis
    use mpp_mod     ! Includes MPI header
    implicit none
    
    integer :: oasis_comp_id
    character (len=6), parameter :: oasis_comp_name = 'datatm'
    
    !------------
    type cpl_data
        ! Do I need this?
        
        integer :: var_id
        integer :: partition_id
    
    end type cpl_data
 
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
    subroutine cpl_partition_init(p_type, p_length, partition_id)
        ! TODO: add optional arguments for other partition types
        
        character(len=*), intent(in) :: p_type
        integer, intent(in) :: p_length
        integer, intent(out) :: partition_id
        
        select case(p_type)
            case ('serial')
                call cpl_partition_serial_init(p_length, partition_id)
            
            case ('apple')
                print *, 'Apple is unimplemented'
                stop(-1)
            
            case ('box')
                print *, 'Box is unimplemented'
                stop(-1)
            
            case ('orange')
                print *, 'Orange is unimplemented'
                stop(-1)
            
            case default
                print *, 'Unknown partition type'
                stop(-1)
        end select
        
    end subroutine cpl_partition_init
    
    
    !--------------------------------------------------------
    subroutine cpl_partition_serial_init(p_length, partition_id)
        
        integer, intent(in) :: p_length
        integer, intent(out) :: partition_id
        
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
    
    end subroutine cpl_partition_serial_init
    
    
    !---
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
        integer, parameter :: var_tx_type = OASIS_In
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
