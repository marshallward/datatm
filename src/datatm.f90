! datatm: A data-driven atmospheric model
!   This delivers atmospheric data to a target ocean model via OASIS
! 
! Contact: Marshall Ward <marshall.ward@gmail.com>

program datatm
    use coupler_mod
    use mpp_mod
    use field_mod
    implicit none
    
    !----------
    ! Test data
    integer :: u_cpl_id, u_part_id
    integer, dimension(3) :: ig_paral
    integer :: ierr
    
    ! TODO: Make a oasis_cpl data type
    character(len=*), parameter :: u_cpl_name = 'U_10WIND'
    integer, dimension(2), parameter :: u_cpl_dims = [ 2, 1 ]
    integer, parameter :: u_tx = OASIS_In
    integer, dimension(4), parameter :: u_cpl_shape = [ 1, 194, 1, 92 ]
    integer, parameter :: u_cpl_dtype = OASIS_Real
    
    !- field.f90 test
    type(field_manifest) :: fields
    type(field_data) :: x
    integer :: i
    !----------
    
    call coupler_init
    call mpp_init
    
    !- field.f90 test
    call field_init('u10.cpl.nc', 'U_10', x)
    
    call field_manifest_init(fields)
    call field_register(fields, x)
    call field_register(fields, x)
    call field_register(fields, x)
    call field_register(fields, x)
    
    call field_manifest_terminate(fields)
    !----
    
    ! Define fields
    ig_paral(1) = 0         ! serial
    ig_paral(2) = 0         ! offset (unused)
    ig_paral(3) = 194 * 92  ! Grid size
    
    call oasis_def_partition(u_part_id, ig_paral, ierr)
    if (ierr /= OASIS_Ok) then
        call oasis_abort(oasis_comp_id, oasis_comp_name, 'partition oops')
    endif
    
    call oasis_def_var(u_cpl_id, u_cpl_name, u_part_id, u_cpl_dims, &
                       u_tx, u_cpl_shape, u_cpl_dtype, ierr)
    if (ierr /= OASIS_Ok) then
        call oasis_abort(oasis_comp_id, oasis_comp_name, 'def_var oops')
    endif
    
    call oasis_enddef(ierr)
    if (ierr /= OASIS_Ok) then
        call oasis_abort(oasis_comp_id, oasis_comp_name, 'enddef oops')
    endif
    
    ! put/get calls
    
    call coupler_exit
 
end program datatm
