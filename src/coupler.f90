module coupler_mod
    use mod_oasis
    use mpp_mod     ! Includes MPI header
    implicit none
    
    integer :: oasis_comp_id
    character (len=6), parameter :: oasis_comp_name = 'datatm'

contains
    
    subroutine coupler_init
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
    end subroutine coupler_init
    
    
    subroutine coupler_exit
        character (len=*), parameter :: method_name = 'coupler_exit'
        integer :: ierr
        
        call oasis_terminate(ierr)
        if (ierr /= OASIS_OK) then
            call oasis_abort(oasis_comp_id, method_name, 'STOP 1')
        endif
        
        ! Manually terminate MPI until that vayu bug is sorted out
        call MPI_Finalize(ierr)
        if (ierr /= MPI_SUCCESS) stop(-1)
    end subroutine coupler_exit

end module coupler_mod
