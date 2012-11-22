! datatm: A data-driven atmospheric model
!   This delivers atmospheric data to a target ocean model via OASIS
! 
! Contact: Marshall Ward <marshall.ward@gmail.com>
program datam
    use coupler_mod
    
    implicit none
    
    integer :: local_comm
    
    call coupler_init (local_comm)
    call coupler_exit
    
end program
