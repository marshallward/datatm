! String manipulation functions (WTB fortran STL)

module str_mod
    implicit none

contains
    function str_lower(str_in) result(str_out)
        ! NOTE: Some compilers might not allow len=len(str_in)

        character(len=*), intent(in) :: str_in
        character(len=len(str_in)) :: str_out

        integer :: i, c
        do i = 1, len(str_in)
            c = iachar(str_in(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) then
                str_out(i:i) = achar(iachar(str_in(i:i)) + 32)
            else
                str_out(i:i) = str_in(i:i)
            end if
        end do

    end function str_lower

end module str_mod
