module qplot_m
    
    use ifqwin
    
    implicit none
!    
!    integer(2), parameter :: plot_title_len = 40
!    integer, parameter :: dp = selected_real_kind(15)
!    
!    
!    type qplot_t
!        integer(2) :: height, width
!        real(8) :: xmin, xmax, ymin, ymax
!        integer(4) :: bgcolor, txtcolor, axiscolor
!        character(len=plot_title_len) :: title
!        integer(4) :: uplot, ulegend
!    end type
!    
!    
!    ! line patterns (styles)
!    integer(2), parameter :: ls_solid = z'ffff', ls_dash = z'eeee', &
!        ls_dashdot = z'ecec', lsdashdotdot = z'eccc', ls_dot = z'aaaa'
!    
!    interface plot
!        procedure :: plot_array, plot_function
!    end interface plot
!    
!    interface setxaxis
!        procedure :: setxaxis_lim, setxaxis_pts
!    end interface
!
!    interface setyaxis
!        procedure :: setyaxis_lim, setyaxis_pts
!    end interface
!    
!    abstract interface
!        function univarfun(x)
!            import :: dp
!            real(dp), intent(in) :: x
!            real(dp) :: univarfun
!        end function
!    end interface
!    
!contains
!    
!    function newplot(desc, width, height, xmin, xmax, ymin, ymax, title, bgcolor, txtcolor, legend)
!    
!        integer, intent(in) :: width, height
!        real(dp), intent(in) :: xmin, xmax, ymin, ymax
!        character(len=*), intent(in) :: title
!        integer, intent(in), optional :: bgcolor, txtcolor
!        logical, optional :: legend
!        integer, intent(out) :: desc
!        
!        type(qplot_t) :: newplot
!        
!        integer :: istatus, bgcola, txtcola
!        real(dp) :: xpad, ypad
!        logical :: lstatus
!        type(windowconfig) :: wc
!        type(qwinfo) :: winfo
!        
!        integer(2) :: fontnum, numfonts
!                
!        if ( present(bgcolor) ) then
!            bgcola = bgcolor
!        else
!            bgcola = colwhite
!        end if
!        
!        if ( present(txtcolor) ) then
!            txtcola = txtcolor
!        else
!            txtcola = colblack
!        end if
!            
!        wc%numxpixels = width
!        wc%numypixels = height
!        wc%numtextcols = -1
!        wc%numtextrows = -1
!        wc%numcolors = -1
!        wc%title = title
!        wc%fontsize = #008000e
!    
!        open (newunit=desc, file = 'user')
!        lstatus = focusqq(desc)
!        lstatus = setwindowconfig(wc)
!        if (.not. lstatus) lstatus = setwindowconfig(wc)
!    
!        istatus = setbkcolorrgb(bgcola)
!        istatus = settextcolorrgb(txtcola)
!        
!        numfonts = initializefonts()
!        ! set typeface to arial, character height to 18,
!        ! character width to 10, and italic
!        fontnum = setfont('t''arial''h10w6b')
!
!        
!        call clearscreen($gclearscreen)
!   
!        ! status = getwsizeqq(10, qwin$sizecurr, winfo)
!    
!        ! set window coordinates
!        lstatus = displaycursor(.false.)
!        ! set padding
!        xpad = (xmax - xmin) * 0.1_dp
!        ypad = (ymax - ymin) * 0.1_dp
!        
!        lstatus = setwindow(.true., xmin - xpad, ymin - 2*ypad, xmax + xpad, ymax + ypad)
!    
!    end function newplot
!    
!    
!    
!    
!    subroutine setxaxis_lim(desc, xmin, xmax, y, color, ntics)
!        
!        integer, intent(in) :: desc
!        real(dp), intent(in) :: xmin, xmax
!        real(dp), intent(in), optional :: y
!        integer, intent(in), optional :: color
!        integer, intent(in), optional :: ntics
!        
!        real(dp) :: yy = 0d0, dx, x
!        integer(4) :: col, nt = 5, i, i4stat, oldcolor
!        integer(2) :: i2stat
!        type(wxycoord) :: oldpos, oldpos1
!        type(xycoord) :: pos1, pos2
!        character(len=10) :: xstr
!        
!        
!        if ( present(y) ) yy = y
!        
!        if ( present(color) ) then
!            col = color
!        else
!            col = gettextcolorrgb()
!        end if
!        
!        if ( present(ntics) ) nt = ntics
!        
!        i4stat = focusqq(desc)
!        oldcolor = setcolorrgb(col)
!        
!        call moveto_w(xmin, yy, oldpos)
!        i2stat = lineto_w(xmax, yy)
!        
!        dx = (xmax - xmin) / (nt - 1)
!        x = xmin
!        
!        do i = 1, nt
!            call moveto_w(x, yy, oldpos1)
!            call getcurrentposition(pos1)
!            call moveto(pos1%xcoord - 10, pos1%ycoord+10, pos2)
!            write(xstr, '(g10.3)') x
!            call outgtext(xstr)
!            call moveto(pos1%xcoord, pos1%ycoord, pos2)
!            i2stat = lineto(pos1%xcoord, pos1%ycoord + 3_2)
!            x = x + dx
!        end do
!        
!        call moveto_w(oldpos%wx, oldpos%wy, oldpos1)
!        oldcolor = setcolorrgb(oldcolor)
!        
!    end subroutine setxaxis_lim
!    
!    
!    
!    
!    subroutine setxaxis_pts(desc, xtics, y, color)
!        
!        integer, intent(in) :: desc
!        real(dp), dimension(:), intent(in) :: xtics
!        real(dp), intent(in), optional :: y
!        integer, intent(in), optional :: color
!        
!        integer :: n, col, i
!        integer(4) :: i4stat, oldcolor
!        integer(2) :: i2stat
!        real(dp) :: yy = 0d0
!        type(wxycoord) :: oldpos, oldpos1
!        type(xycoord) :: pos1, pos2
!        character(len=10) :: xstr
!        
!        n = size(xtics)
!        
!        if ( present(y) ) yy = y
!        
!        if ( present(color) ) then
!            col = color
!        else
!            col = gettextcolorrgb()
!        end if
!        
!        i4stat = focusqq(desc)
!        oldcolor = setcolorrgb(col)
!        
!        call moveto_w(xtics(1), yy, oldpos)
!        i2stat = lineto_w(xtics(n), yy)
!        
!        do i = 1, n
!            call moveto_w(xtics(i), yy, oldpos1)
!            call getcurrentposition(pos1)
!            call moveto(pos1%xcoord - 10, pos1%ycoord+10, pos2)
!            write(xstr, '(g10.3)') xtics(i)
!            call outgtext(xstr)
!            call moveto(pos1%xcoord, pos1%ycoord, pos2)
!            i2stat = lineto(pos1%xcoord, pos1%ycoord + 3_2)
!        end do
!        
!        oldcolor = setcolorrgb(oldcolor)
!        
!    end subroutine setxaxis_pts
!    
!    
!    subroutine setyaxis_lim(desc, ymin, ymax, x, color, ntics)
!        
!        integer, intent(in) :: desc
!        real(dp), intent(in) :: ymin, ymax
!        real(dp), intent(in), optional :: x
!        integer, intent(in), optional :: color
!        integer, intent(in), optional :: ntics
!        
!        real(dp) :: xx = 0d0, dy, y
!        integer :: col, nt = 5, i
!        logical :: lstatus
!        type(wxycoord) :: oldpos, oldpos1
!        type(xycoord) :: pos1, pos2
!        character(len=10) :: ystr
!        
!        
!        if ( present(x) ) xx = x
!        
!        if ( present(color) ) then
!            col = color
!        else
!            col = gettextcolorrgb()
!        end if
!        
!        if ( present(ntics) ) nt = ntics
!        
!        lstatus = focusqq(desc)
!        lstatus = setcolorrgb(col)
!        
!        call moveto_w(xx, ymin, oldpos)
!        lstatus = lineto_w(xx, ymax)
!        
!        dy = (ymax - ymin) / (nt - 1)
!        y = ymin
!        
!        do i = 1, nt
!            call moveto_w(xx, y, oldpos1)
!            call getcurrentposition(pos1)
!            call moveto(pos1%xcoord - 50, pos1%ycoord-5, pos2)
!            write(ystr, '(g10.3)') y
!            call outgtext(ystr)
!            call moveto(pos1%xcoord, pos1%ycoord, pos2)
!            lstatus = lineto(pos1%xcoord-3_2, pos1%ycoord)
!            y = y + dy
!        end do
!        
!        call moveto_w(oldpos%wx, oldpos%wy, oldpos1)
!        
!    end subroutine setyaxis_lim
!    
!    
!    
!    subroutine setyaxis_pts(desc, ytics, x, color)
!        
!        integer, intent(in) :: desc
!        real(dp), dimension(:), intent(in) :: ytics
!        real(dp), intent(in), optional :: x
!        integer, intent(in), optional :: color
!        
!        integer :: n, col, i
!        integer(4) :: i4stat, oldcolor
!        integer(2) :: i2stat
!        real(dp) :: xx = 0d0
!        type(wxycoord) :: oldpos, oldpos1
!        type(xycoord) :: pos1, pos2
!        character(len=10) :: xstr
!        
!        n = size(ytics)
!        
!        if ( present(x) ) xx = x
!        
!        if ( present(color) ) then
!            col = color
!        else
!            col = gettextcolorrgb()
!        end if
!        
!        i4stat = focusqq(desc)
!        oldcolor = setcolorrgb(col)
!        
!        call moveto_w(xx, ytics(1), oldpos)
!        i2stat = lineto_w(xx, ytics(n))
!        
!        do i = 1, n
!            call moveto_w(xx, ytics(i), oldpos1)
!            call getcurrentposition(pos1)
!            call moveto(pos1%xcoord - 50_2, pos1%ycoord-5_2, pos2)
!            write(xstr, '(g10.3)') ytics(i)
!            call outgtext(xstr)
!            call moveto(pos1%xcoord, pos1%ycoord, pos2)
!            i2stat = lineto(pos1%xcoord-3_2, pos1%ycoord)
!        end do
!        
!        oldcolor = setcolorrgb(oldcolor)
!        
!    end subroutine setyaxis_pts
!    
!    
!    subroutine plot_array(desc, x, y, color, joined)
!        
!        integer, intent(in) :: desc, color
!        real(dp), dimension(:), intent(in) :: x, y
!        logical, intent(in), optional :: joined
!        
!        integer(4) :: n, i, oldcol, i4stat
!        integer(2) :: i2stat
!        logical :: lstatus, join = .true.
!        type(wxycoord) :: oldpos, oldpos1
!        type(xycoord) :: cpos
!        real(dp) :: dx, dy
!        integer(1), dimension(8) :: bmask
!    
!        n = size(x)
!        
!        if ( present(joined) ) join = joined
!    
!        i4stat = focusqq(desc)
!        oldcol = setcolorrgb(color)
!        ! save current position
!        call moveto_w(0d0, 0d0, oldpos)
!        
!        if (join) then
!            do i=1,n
!                call moveto_w(x(i), y(i), oldpos1)
!                i2stat = lineto_w(x(i+1), y(i+1))
!            end do
!        else
!            bmask(:) = z'ff'
!            call setfillmask(bmask)
!            do i=1,n
!                call moveto_w(x(i), y(i), oldpos1)
!                call getcurrentposition(cpos)
!                i2stat = ellipse($gfillinterior, &
!                    cpos%xcoord - 2_2, cpos%ycoord - 2_2, &
!                    cpos%xcoord + 2_2, cpos%ycoord + 2_2)
!            end do
!        end if
!        ! restore last postion
!        call moveto_w(oldpos%wx, oldpos%wy, oldpos1)
!        oldcol = setcolorrgb(oldcol)
!        
!    end subroutine plot_array
!    
!    
!    
!    
!    subroutine plot_function(desc, fun, xmin, xmax, color, nsamples)
!    
!        integer, intent(in) :: desc, color
!        real(dp) :: xmin, xmax
!        integer, optional, intent(in) :: nsamples
!        
!        procedure(univarfun), pointer, intent(in) :: fun
!        
!        type(wxycoord) :: oldpos, oldpos1
!        
!        integer :: i, n = 200
!        logical :: lstatus
!        real(dp) :: x, dx, y
!        
!        if ( present(nsamples) ) n = nsamples
!        
!        dx = (xmax - xmin) / n
!        
!        lstatus = focusqq(desc)
!        lstatus = setcolorrgb(color)
!        call moveto_w(xmin, fun(xmin), oldpos)
!        x = xmin
!        
!        do i=1,n
!            x = x + dx
!            y = fun(x)
!            lstatus = lineto_w(x, y)
!            call moveto_w(x, y, oldpos1)
!        end do
!        
!        call moveto_w(oldpos%wx, oldpos%wy, oldpos1)
!        
!    
!    end subroutine plot_function
!    
end module qplot_m