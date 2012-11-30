! datatm: A data-driven atmospheric model
!   This delivers atmospheric data to a target ocean model via OASIS
! 
! Contact: Marshall Ward <marshall.ward@gmail.com>
program datam
    use coupler_mod
    use mpp_mod
    implicit none
    
    ! Testing
    integer :: part_id
    integer, dimension(3) :: ig_paral
    
    call coupler_init
    call mpp_init
    
    ! Define fields
    ig_paral(1) = 0         ! serial
    ig_paral(2) = 0         ! offset (unused)
    ig_paral(3) = 92 * 194  ! Grid size
    call oasis_def_partition(part_id, ig_paral, ierr)
    
    ! put/get calls
    
    call coupler_exit
    
end program
