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

    ! cpl test
    integer :: p_serial
    integer :: cpl_id

    !- field.f90 test
    type(field_manifest) :: fields
    type(field_data) :: u10
    type(field_data) :: v10
    !----------

    integer :: t_step, t_end
    integer :: i

    !---
    call cpl_init
    call mpp_init

    ! testing
    t_end = 2
    call cpl_partition_init('serial', 192*94, p_serial)

    call field_init('u10.cpl.nc', 'U_10', 'U_10WIND', p_serial, u10)
    call field_init('v10.cpl.nc', 'V_10', 'V_10WIND', p_serial, v10)

    call field_manifest_init(fields)

    call field_register(fields, u10)
    call field_register(fields, v10)

    call cpl_init_finalize

    ! For now, assume all fields are on the same time axis
    ! TODO: Collate time axis from all fields and construct a "master" time axis

    !---
    ! Model loop here
    do t_step = 1, t_end

        ! Need to convert t_step to a physical time (in seconds)

        ! Update manifest fields
        call field_update_manifest(fields, t_step)
        call field_manifest_push(fields, t_step)

    end do

    !---
    call field_manifest_terminate(fields)
    call cpl_exit

end program datatm
