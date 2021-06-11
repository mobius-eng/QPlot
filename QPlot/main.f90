program qplotapp
    
    use ifqwin
    use canvas_m
    implicit none
    integer newwin
    logical lstatus
    type(canvas_t) cnv
    type(windowconfig) :: wc
    type(qwinfo) :: winfo
    
    wc%numxpixels = 800
    wc%numypixels = 600
    wc%numtextcols = -1
    wc%numtextrows = -1
    wc%numcolors = -1
    wc%title = 'Test fonts'
    wc%fontsize = #008000e

    open (newunit=newwin, file = 'user')
    lstatus = focusqq(newwin)
    lstatus = setwindowconfig(wc)
    if (.not. lstatus) lstatus = setwindowconfig(wc)
    call test_font(newwin, 'Terminus (TTF) for Windows')
    
    call cnv%init("My example", 800, 600, -1.0_8, 1.0_8, -1.0_8, 1.0_8, .true.)
    call cnv%set_foreground_color(red)
    call cnv%set_background_color(blue)
    call cnv%draw_line(-1.0_8, -1.0_8, 1.0_8, 1.0_8)
    call cnv%draw_ellipse(0.0_8, 0.0_8, 0.5_8, 0.5_8)
    call cnv%fill_ellipse(0.0_8, 0.0_8, -0.5_8, -0.5_8)
    call cnv%draw_rectangle(0.0_8, 0.0_8, -0.5_8, -0.5_8)
    call cnv%set_foreground_color(black)
    call cnv%set_linestyle(linestyle_dashdot_index)
    call cnv%draw_lines([-1.0_dp, -1.0_dp, 0.0_dp], [1.0_dp, 0.0_dp, 1.0_dp])
    call cnv%set_text_color(green)
    call cnv%set_font('Terminus (TTF) for Windows', 14)
    call cnv%draw_text(0.0_dp, 0.0_dp, 'Hello World!')
contains

    subroutine test_font(win_unit, font)
        integer, intent(in) :: win_unit
        character(len=*), intent(in) :: font
        integer :: istat
        character(len=256) :: font_format
        type(xycoord) :: t, t2
        write (font_format, '("t''", A, "''h14b")') trim(font)       
        write (win_unit, *) trim(font_format)
        istat = initializefonts()
        if (istat <= 0) then
            write (win_unit, *) 'Cannot initialize fonts'
            return
        end if
        ! set typeface to arial, character height to 18,
        ! character width to 10, and italic
        istat = setfont(trim(font_format))
        if (istat < 0) then
            write (win_unit, *) 'Cannot set selected font', trim(font)
            return
        end if
        call moveto(50, 200, t)
        call outgtext('This is written with the selected font')
        call moveto(t%xcoord, t%ycoord, t2)
    end subroutine
end program
    