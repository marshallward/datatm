! datatm: A data-driven atmospheric model
!   This delivers atmospheric data to a target ocean model via OASIS
! 
! Contact: Marshall Ward <marshall.ward@gmail.com>

program datatm
    use cpl_mod
    use mpp_mod
    use field_mod
    implicit none
    
    !----------
    ! Test data
    integer :: u_cpl_id
    integer :: ierr
    
    ! TODO: Make a oasis_cpl data type
    character(len=*), parameter :: u_cpl_name = 'U_10WIND'
    integer, dimension(2), parameter :: u_cpl_dims = [ 2, 1 ]
    integer, parameter :: u_tx = OASIS_In
    integer, dimension(4), parameter :: u_cpl_shape = [ 1, 194, 1, 92 ]
    integer, parameter :: u_cpl_dtype = OASIS_Real
    
    ! cpl test
    integer :: p_serial
    integer :: cpl_id
    
    !- field.f90 test
    type(field_manifest) :: fields
    type(field_data) :: x
    integer :: i
    !----------
    
    call cpl_init
    call mpp_init
    
    ! cpl test
    call cpl_partition_init('serial', size(x%val), p_serial)
    
    !- field test
    call field_init('u10.cpl.nc', 'U_10', 'U_10WIND', p_serial, x)
    
    print *, "size", size(x%val)
    print *, "shape", shape(x%val)
    print *, "rank", shape(shape(x%val))

    call field_manifest_init(fields)
    call field_register(fields, x)
    call field_register(fields, x)
    call field_register(fields, x)
    call field_register(fields, x)
    
    call field_manifest_terminate(fields)
    !----
    
    call oasis_enddef(ierr)
    if (ierr /= OASIS_Ok) then
        call oasis_abort(oasis_comp_id, oasis_comp_name, 'enddef oops')
    endif
    
    call cpl_exit
 
end program datatm
