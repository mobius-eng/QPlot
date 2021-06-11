module canvas_m
    
    use ifqwin
    use iso_c_binding, only: c_int32_t, c_int8_t, c_null_char
    implicit none
    
    ! Intel Fortran double precision - only for IFQWIN procedures
    integer, parameter, private :: ifdp = 8
    integer, parameter :: dp = selected_real_kind(15)
    ! Intel Fortran integer types - only for IFQWIN procedures
    integer, parameter, private :: ifint1 = 1, ifint2 = 2, ifint4 = 4
    ! Test the assumption about integer sizes: both const must be 0
    integer, parameter, private :: int_size_test1 = c_int32_t - ifint4, &
        &   int_size_test2 = ifint4 - c_int32_t
    ! This will break the compilation if the assumption is wrong
    real(ifdp), parameter :: test1 = sqrt(real(int_size_test1, kind = ifdp)), &
        & test2 = sqrt(real(int_size_test2, kind = ifdp))
    ! Similar test for ifdp & dp
    integer, parameter, private :: real_kind_test1 = ifdp - dp, real_kind_test2 = dp - ifdp
    real(ifdp), parameter, private :: real_kind1 = sqrt(real(real_kind_test1, ifdp)), &
        &   real_kind2 = sqrt(real(real_kind_test2, ifdp))
    ! line patterns (styles)
    integer(ifint2), parameter, private :: &
        & ls_solid = z'ffff', ls_dash = z'eeee', ls_dashdot = z'ecec', &
        & ls_dashdotdot = z'eccc', ls_dot = z'aaaa'
    integer(ifint2), parameter, dimension(5) :: linestyles = [ls_solid, ls_dash, ls_dashdot, ls_dashdotdot, ls_dot]
    integer(c_int8_t), parameter ::                                 &
        &   linestyle_solid_index = 1,                              &
        &   linestyle_dash_index = 2,                               &
        &   linestyle_dashdot_index = 3,                            &
        &   linestyle_dashdotdot_index = 4,                         &
        &   linestyle_dot_index = 5
    
    type color_t
        integer(ifint4), private :: rgb
    contains
        procedure, pass(self) :: set_num  => color_set_num
        procedure, pass(self) :: set_comp => color_set_comp
        generic               :: set      => set_num, set_comp
        procedure, pass(self) :: get      => color_get
        procedure, pass(self) :: red      => color_red
        procedure, pass(self) :: green    => color_green
        procedure, pass(self) :: blue     => color_blue
    end type
    
    type canvas_t
        type(color_t) :: background_color
        type(color_t) :: foreground_color
        type(color_t) :: text_color
        integer(c_int8_t) :: linestyle_index
        integer(1), dimension(8) :: fill_mask
        integer :: window_unit
    contains
        procedure, pass(self) :: init => canvas_init
        procedure, pass(self) :: set_background_color => canvas_set_background_color
        procedure, pass(self) :: set_foreground_color => canvas_set_foreground_color
        procedure, pass(self) :: set_text_color => canvas_set_text_color
        procedure, pass(self) :: draw_line => canvas_draw_line
        procedure, pass(self) :: draw_lines => canvas_draw_lines
        procedure, pass(self) :: draw_ellipse => canvas_draw_ellipse
        procedure, pass(self) :: fill_ellipse => canvas_fill_ellipse
        procedure, pass(self) :: draw_rectangle => canvas_draw_rectangle
        procedure, pass(self) :: fill_rectangle => canvas_fill_rectangle
        procedure, pass(self) :: draw_polygon => canvas_draw_polygon
        procedure, pass(self) :: fill_polygon => canvas_fill_polygon
        procedure, pass(self) :: draw_text => canvas_draw_text
        procedure, pass(self) :: set_font => canvas_set_font
        procedure, pass(self) :: set_linestyle => canvas_set_linestyle
    end type
    ! Pre-defined colors
    type(color_t), parameter ::                 &
        &   red = color_t(z'ff'),               &
        &   green = color_t(z'ff00'),           &
        &   blue = color_t(z'ff0000'),          &
        &   grey = color_t(z'888888'),          &
        &   light_grey = color_t(z'cccccc'),    &
        &   dark_grey = color_t(z'444444'),     &
        &   black = color_t(z'000000'),         &
        &   white = color_t(z'ffffff'),         &
        &   orange = color_t(z'ffa500')


contains
    !--------------------------------------------------------------------------
    ! Color subroutines
    !--------------------------------------------------------------------------
    subroutine color_set_num(self, rgb)
        class(color_t), intent(inout) :: self
        integer(c_int32_t), intent(in) :: rgb
        self%rgb = rgb
    end subroutine
    
    subroutine color_set_comp(self, red, green, blue)
        class(color_t), intent(inout) :: self
        integer(c_int8_t), intent(in) :: red, green, blue
        self%rgb = lshift(blue, 16) + lshift(green, 8) + red
    end subroutine
    
    pure integer(c_int32_t) function color_get(self)
        class(color_t), intent(in) :: self
        color_get = self%rgb
    end function
    
    pure integer(c_int8_t) function color_red(self) result (c)
        class(color_t), intent(in) :: self
        c = iand(self%rgb, z'ff')
    end function
    
    pure integer(c_int8_t) function color_green(self) result (c)
        class(color_t), intent(in) :: self
        c = lshift(iand(self%rgb, z'ff00'), -8)
    end function
    
    pure integer(c_int8_t) function color_blue(self) result (c)
        class(color_t), intent(in) :: self
        c = lshift(iand(self%rgb, z'ff0000'), -16)
    end function
    !--------------------------------------------------------------------------
    ! Canvas subroutines
    !--------------------------------------------------------------------------
    subroutine canvas_init(self, title, width, height, xmin, xmax, ymin, ymax, &
                           yinverse)
        class(canvas_t), intent(inout) :: self
        integer, intent(in) :: width, height
        real(dp), intent(in) :: xmin, xmax, ymin, ymax
        character(len=*), intent(in) :: title
        logical, intent(in) :: yinverse
        integer :: istatus
        real(dp) :: xpad, ypad
        logical :: lstatus
        type(windowconfig) :: wc
        type(qwinfo) :: winfo 
        integer(2) :: fontnum, numfonts
        ! To satisfy the interface
        logical(2) :: yinverse2
        wc%numxpixels = width
        wc%numypixels = height
        wc%numtextcols = -1
        wc%numtextrows = -1
        wc%numcolors = -1
        wc%title = trim(title) // c_null_char
        ! Have to provide it here
        wc%fontsize = #008000e
        open (newunit=self%window_unit, file = 'user')
        lstatus = focusqq(self%window_unit)
        lstatus = setwindowconfig(wc)
        ! In case WC spec cannot be used - use the closest available
        if (.not. lstatus) lstatus = setwindowconfig(wc)
        ! Set default colors
        call self%set_background_color(white)
        call self%set_foreground_color(black)
        call self%set_text_color(black)
        ! Set default line style
        call setlinestyle(linestyles(linestyle_solid_index))
        ! Initialize fonts
        numfonts = initializefonts()
        call clearscreen($gclearscreen)
        lstatus = displaycursor(.false.)
        ! set padding
        xpad = (xmax - xmin) * 0.1_ifdp
        ypad = (ymax - ymin) * 0.1_ifdp
        ! Set window coordinates
        yinverse2 = yinverse
        lstatus = setwindow(yinverse2, xmin - xpad, ymin - 2*ypad, &
            &               xmax + xpad, ymax + ypad)    
    end subroutine
    
    subroutine canvas_draw_line(self, x1, y1, x2, y2)
        class(canvas_t), intent(in) :: self
        real(dp), intent(in) :: x1, y1, x2, y2
        type(wxycoord) dummy
        integer(ifint2) iresult2
        integer(ifint4) iresult4
        iresult4 = setactiveqq(self%window_unit)
        call moveto_w(x1, y1, dummy)
        iresult2 = lineto_w(x2, y2)
    end subroutine
    
    subroutine canvas_draw_lines(self, x, y)
        class(canvas_t), intent(inout) :: self
        real(dp), intent(in), dimension(:) :: x, y
        integer :: nx, ny, n, i
        nx = size(x)
        ny = size(y)
        n = min(nx, ny)
        do i = 1, n - 1
            call self%draw_line(x(i), y(i), x(i+1), y(i+1))
        end do
    end subroutine
    
    subroutine canvas_draw_polygon(self, x, y)
        class(canvas_t), intent(inout) :: self
        real(dp), intent(in), dimension(:) :: x, y
        integer :: nx, ny, n, i
        nx = size(x)
        ny = size(y)
        n = min(nx, ny)
        ! It's esiear to draw polygon directly -- no need for
        ! additional arrays of WXYCOORD.
        ! Cannot fill polygon like that though
        do i = 1, n - 1
            call self%draw_line(x(i), y(i), x(i+1), y(i+1))
        end do
        call self%draw_line(x(n), y(n), x(1), y(1))
    end subroutine
    
    subroutine canvas_fill_polygon(self, x, y)
        class(canvas_t), intent(inout) :: self
        real(dp), intent(in), dimension(:) :: x, y
        integer :: nx, ny, n, i, iresult
        type(wxycoord), dimension(:), allocatable :: points
        integer(ifint4) iresult4
        iresult4 = setactiveqq(self%window_unit)
        nx = size(x)
        ny = size(y)
        n = min(nx, ny)
        allocate(points(n))
        do i = 1, n
            points(i)%wx = x(i)
            points(i)%wy = y(i)
        end do
        iresult = polygon_w($gfillinterior, points, n)
    end subroutine
    
    subroutine canvas_draw_ellipse(self, x1, y1, x2, y2)
        class(canvas_t), intent(in) :: self
        real(dp), intent(in) :: x1, y1, x2, y2
        integer(ifint2) iresult2
        integer(ifint4) iresult4
        iresult4 = setactiveqq(self%window_unit)
        iresult2 = ellipse_w($gborder, x1, y1, x2, y2)
    end subroutine
    
    subroutine canvas_fill_ellipse(self, x1, y1, x2, y2)
        class(canvas_t), intent(in) :: self
        real(dp), intent(in) :: x1, y1, x2, y2
        integer(ifint2) iresult2
        integer(ifint4) iresult4, prevcolor
        iresult4 = setactiveqq(self%window_unit)
        ! bkcolor in ifqwin doesn't mean what you think it means.
        ! Hence, this to fill ellipse with background color
        ! fill_* functions do not draw border.
        prevcolor = setcolorrgb(self%background_color%get())
        iresult2 = ellipse_w($gfillinterior, x1, y1, x2, y2)
        prevcolor = setcolorrgb(prevcolor)
    end subroutine
    
    subroutine canvas_set_background_color(self, color)
        class(canvas_t), intent(inout) :: self
        type(color_t), intent(in) :: color
        integer(ifint4) iresult4
        integer istatus
        iresult4 = setactiveqq(self%window_unit)
        self%background_color = color
        istatus = setbkcolorrgb(color%get())
    end subroutine
    
    subroutine canvas_set_foreground_color(self, color)
        class(canvas_t), intent(inout) :: self
        type(color_t), intent(in) :: color
        integer(ifint4) iresult4
        integer istatus
        iresult4 = setactiveqq(self%window_unit)
        self%foreground_color = color
        istatus = setcolorrgb(color%get())
    end subroutine
    
    subroutine canvas_set_text_color(self, color)
        class(canvas_t), intent(inout) :: self
        type(color_t), intent(in) :: color
        integer(ifint4) iresult4
        integer istatus
        iresult4 = setactiveqq(self%window_unit)
        self%text_color = color
        istatus = settextcolorrgb(color%get())
    end subroutine
        
    subroutine canvas_draw_rectangle(self, x1, y1, x2, y2)
        class(canvas_t), intent(in) :: self
        real(dp), intent(in) :: x1, y1, x2, y2
        integer(ifint2) iresult2
        integer(ifint4) iresult4
        iresult4 = setactiveqq(self%window_unit)
        iresult2 = rectangle_w($gborder, x1, y1, x2, y2)
    end subroutine
    
    subroutine canvas_fill_rectangle(self, x1, y1, x2, y2)
        class(canvas_t), intent(in) :: self
        real(dp), intent(in) :: x1, y1, x2, y2
        integer(ifint2) iresult2
        integer(ifint4) iresult4, prevcolor
        iresult4 = setactiveqq(self%window_unit)
        ! bkcolor in ifqwin doesn't mean what you think it means.
        ! Hence, this to fill ellipse with background color
        ! fill_* functions do not draw border.
        prevcolor = setcolorrgb(self%background_color%get())
        iresult2 = rectangle_w($gfillinterior, x1, y1, x2, y2)
        prevcolor = setcolorrgb(prevcolor)
    end subroutine
    
    subroutine canvas_draw_text(self, x, y, text, rotate)
        class(canvas_t), intent(inout) :: self
        real(dp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        integer(c_int32_t), intent(in), optional :: rotate
        type(wxycoord) dummy
        integer(ifint4) iresult4
        iresult4 = setactiveqq(self%window_unit)
        if (present(rotate)) then
            call setgtextrotation(rotate)
        end if
        call moveto_w(x, y, dummy)
        call outgtext(text)
        call setgtextrotation(0_c_int32_t)
    end subroutine

    subroutine canvas_set_font(self, font_name, font_height)
        class(canvas_t), intent(inout) :: self
        character(len=*), intent(in) :: font_name
        integer(c_int32_t), intent(in) :: font_height
        character(len=256) :: font_format
        character(len=4) :: height
        integer istat
        write(height, '(I4)') font_height
        write (font_format, '("t''", A, "''h",A,"b")') trim(font_name), adjustl(height)
        istat = setfont(trim(font_format))
    end subroutine
    
    subroutine canvas_set_linestyle(self, linestyle_index)
        class(canvas_t), intent(inout) :: self
        integer(c_int32_t), intent(in) :: linestyle_index
        if (linestyle_index < 1 .or. linestyle_index > 5) return
        call setlinestyle(linestyles(linestyle_index))
    end subroutine
    
end module