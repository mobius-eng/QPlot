module composer_m
    
    use iso_c_binding, only: c_int32_t, c_loc, c_ptr, c_f_pointer
    use ifqwin
    use canvas_m
    use list_m
    implicit none
    
    type rect_t
        real(dp) :: x1, y1, x2, y2
    end type
    
    ! COMPOSER_T needs a list of children that are COMPOSER_T
    ! themselves. This creates a problem with using FGL LIST_T
    ! since it needs to know the size of the stored unit. In
    ! fact, we only want to store a pointer anyway. Fortran does
    ! not have first-class-citizen pointers. So, instead, we
    ! have to rely on C_PTR from ISO_C_BINDING and have to
    ! move between C-pointers and Fortran pointers.
    type(c_ptr), private :: c_ptr_dummy 
    integer, parameter :: c_ptr_len = sizeof(c_ptr_dummy)
    
    type, abstract :: composer_t
        class(canvas_t), pointer :: canvas
        type(rect_t) :: bounding_box
        type(list_t(c_ptr_len)) :: children
    contains
        procedure(composer_draw), pass(self), deferred :: draw
    end type
    
    abstract interface
        subroutine composer_draw(self)
            import composer_t
            class(composer_t), intent(inout) :: self
        end subroutine
    end interface
    
    type, extends(composer_t) :: label_t
        character(len = 256) :: text
    contains
        procedure, pass(self) :: init => label_init
        procedure, pass(self) :: draw => label_draw
    end type

contains
    
    subroutine label_init(self, text, x1, y1, x2, y2)
        class(label_t), intent(inout) :: self
        character(len=*) :: text
        real(dp), intent(in) :: x1, y1
        real(dp), intent(in), optional :: x2, y2
        type(fontinfo) :: fntinf
        integer :: iresult
        self%text = trim(text)
        self%bounding_box%x1 = x1
        self%bounding_box%y1 = y1
        if (present(x2)) then
            self%bounding_box%x2 = x2
        else
            self%bounding_box%x2 = x1 + getgtextextent(trim(text))
        end if
        if (present(y2)) then
            self%bounding_box%y2 = y2
        else
            iresult = getfontinfo(fntinf)
            ! Subtract since (x1, y1) is top left corner & y axis is inverted
            self%bounding_box%y2 = y1 - fntinf%pixheight
        end if
    end subroutine

    subroutine label_draw(self)
        class(label_t), intent(inout) :: self
        call self%canvas%draw_text(self%bounding_box%x1, self%bounding_box%y1, &
                    trim(self%text))
    end subroutine
    
    
end module