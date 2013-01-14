! Forcing fields
! Use the module to load and access the model forcing fields

module field_mod
    use io_mod
    use netcdf
    implicit none
    
    
    !--------------
    type field_data
        
        type(io_data) :: io
        !type(oasis_data) :: cpl
        
        real, dimension(:), allocatable :: time
        real, dimension(:, :), allocatable :: val
    end type field_data
    
    
    !------------------
    type field_manifest
        
        integer :: length
        integer :: capacity
        
        type(field_data), dimension(:), allocatable :: f

    end type field_manifest
    
contains
   
    !-------------------------------------------------------
    subroutine field_manifest_init(manifest, initial_length)
        
        type(field_manifest), intent(inout) :: manifest
        integer, intent(in), optional :: initial_length
        
        if (present(initial_length)) then
            manifest%capacity = initial_length
        else
            manifest%capacity = 1
        end if
        
        allocate(manifest%f(manifest%capacity))
        manifest%length = 0
    
    end subroutine field_manifest_init
    
    
    !-----------------------------------------
    subroutine field_register(manifest, field)
        
        type(field_manifest), intent(inout) :: manifest
        type(field_data), intent(in) :: field
        
        integer :: cap_adjust
        type(field_data), dimension(:), allocatable :: f_tmp
        
        if (manifest%length == manifest%capacity) then
            
            ! Use the "CPython" resize formula: 
            !   L_(n+1) <- L_n + L_n / 8 + (L_n < 9) ? 3 : 6
            if (manifest%capacity < 9) then
                cap_adjust = 3
            else
                cap_adjust = 6
            end if
            manifest%capacity = manifest%capacity + manifest%capacity / 8   &
                                + cap_adjust
            
            allocate(f_tmp(manifest%capacity))
            f_tmp = manifest%f
            
            deallocate(manifest%f)
            allocate(manifest%f(manifest%capacity))
            
            manifest%f = f_tmp
            
            deallocate(f_tmp)
        end if
        
        manifest%length = manifest%length + 1
        manifest%f(manifest%length) = field
    
    end subroutine field_register
    
    
    !--------------------------------------------
    subroutine field_manifest_terminate(manifest)
        
        type(field_manifest), intent(inout) :: manifest
        
        integer :: i
        
        do i = 1, manifest%length
            call field_terminate(manifest%f(i))
        end do
        
        deallocate(manifest%f)
    
    end subroutine field_manifest_terminate
    
    
    !----------------------------------------------
    subroutine field_init(filename, varname, field)
        
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: varname
        type(field_data), intent(out) :: field
        
        call io_open(filename, varname, field%io)
        
        call io_allocate_time_axis(field%io, field%time)
        call io_allocate_field(field%io, field%val)
        
        ! TODO: Initialize OASIS metadata
     
    end subroutine field_init
    
    
    !--------------------------------
    subroutine field_terminate(field)
        
        type(field_data), intent(inout) :: field
        
        deallocate(field%time)
        deallocate(field%val)
    
    end subroutine field_terminate
    
end module field_mod
