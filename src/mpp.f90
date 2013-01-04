module mpp_mod
    ! Wrapper to the local MPI environment
    implicit none
    include "mpif.h"
    
    integer :: mpp_comm_id = MPI_COMM_NULL
    integer :: mpp_rank

contains
    subroutine mpp_init
        integer :: ierr, rc
        
        ! Assert that coupler_init has initialised the local MPI comm ID
        if (mpp_comm_id == MPI_COMM_NULL) then
            print *, "Error: Undefined global MPI communicator."
            stop(-1)
        endif
        
        call MPI_Comm_rank(mpp_comm_id, mpp_rank, ierr)
        if (ierr /= MPI_SUCCESS) then
            rc = ierr
            call MPI_Abort(mpp_comm_id, rc, ierr)
        endif
    
    end subroutine mpp_init

end module mpp_mod
