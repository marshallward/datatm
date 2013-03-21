! datatm: A data-driven atmospheric model
!   This delivers atmospheric data to a target ocean model via OASIS
!
! Contact: Marshall Ward <marshall.ward@gmail.com>

! TODO list is too big, here are some problems
!   1. Return codes are all -1 with no messages
!   2. Field input needs some sort of parsing file
!   3. Timestepping is nonexistent

program datatm
    use cpl_mod
    use mpp_mod
    use field_mod
    implicit none

    !----------
    ! Test data

    ! cpl test
    integer :: p_serial, p_box
    integer :: cpl_id

    !- field.f90 test
    type(field_manifest) :: manifest
    type(field_data) :: u10
    type(field_data) :: v10
    !----------

    integer :: t_step, nt_step, dt, date
    real, dimension(:), allocatable :: time
    integer :: i

    !---
    call cpl_init
    call mpp_init

    ! testing
    nt_step = 2
    dt = 21600
    p_serial = cpl_partition_init('serial', 192 * 94)
    p_box = cpl_partition_init('box', 192, 94)

    call field_init('u10.cpl.nc', 'U_10', 'U_10WIND', p_box, u10)
    call field_init('v10.cpl.nc', 'V_10', 'V_10WIND', p_box, v10)
    
    call field_manifest_init(manifest)

    call field_register(manifest, u10)
    call field_register(manifest, v10)

    call cpl_init_finalize

    !---
    ! Model loop here
    do t_step = 1, nt_step

        call field_manifest_update(manifest, t_step)
        
        ! Get the date (in seconds) from the time axis
        date = 10800 + (t_step - 1) * dt

        ! Update manifest fields
        call field_manifest_push(manifest, date)

    end do

    !---
    call field_manifest_terminate(manifest)
    call cpl_exit

end program datatm
