module coupler_mod
    use mod_oasis
    
    implicit none
    
    integer :: comp_oasis_id
    character (len=6), parameter :: comp_oasis_name = 'datatm'

contains
    
    subroutine coupler_init (mpi_localcomm)
        ! Initialise the global MPI communicator and the OASIS coupler
        ! Return the local MPI communicator ID
        integer, intent(out) :: mpi_localcomm
        character (len=*), parameter :: method_name = 'coupler_init'
        integer :: ierr
        
        call oasis_init_comp (comp_oasis_id, comp_oasis_name, ierr)
        if (ierr /= OASIS_OK) then
            call oasis_abort (comp_oasis_id, method_name, 'STOP 1')
        endif
        
        call oasis_get_localcomm (mpi_localcomm)
        if (ierr /= OASIS_OK) then
            call oasis_abort (comp_oasis_id, method_name, 'STOP 2')
        endif
    end subroutine coupler_init


    subroutine coupler_exit
        character (len=*), parameter :: method_name = 'coupler_exit'
        integer :: ierr
        
        call oasis_terminate (ierr)
        if (ierr /= OASIS_OK) then
            call oasis_abort (comp_oasis_id, method_name, 'STOP 1')
        endif
    end subroutine coupler_exit

end module coupler_mod
