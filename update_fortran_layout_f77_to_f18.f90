!########################################################################
! This program gather the list of fortran < *.f90 and store it in a
! for_file_list.txt. This script then read the aforementioned file and 
! modify the layout of these file to produce an harmonised layout, which
! can be compiled *.90, *.95, *.f18, *.f08
!########################################################################
program layout_change
  implicit none

  integer :: i,nlines,status,sn,n,nfiles,pos,pos1,pos2,pos3
  character(len=200) :: line
  character(len=200), allocatable :: lines(:)
  character(len=200),pointer,dimension(:) :: input_file 
  character(len=200) :: output_file,command,fn,ltemp
  logical,dimension(:),allocatable :: add_ampersand

! Gather the list of fortran 77 file in the current directory you are located in.
! Stores this list in a for_file_list.txt.
  command='./filesInDir.sh'
  call system(trim(command))
  
! Read the list of file:
  fn='for_file_list.txt'
  open(20,file=fn)
    read(20,*) nfiles
    allocate(input_file(nfiles))
    do n=1,nfiles
      read(20,'(a)') input_file(n)
    end do
  close(20)
  
! Loop through all the < *.f90 files: 
  do n=1,nfiles

    ! Count the number of lines within the file to allocate correctly 
    ! the string arrays.
    open(10, file=input_file(n), status='old', action='read')
    nlines = 0
    do
       read(10, '(A)', iostat=status) line
       if (status .ne. 0) exit
       nlines = nlines + 1
    end do
    close(10)

    ! Allocate the lines and add ampersand arrays
    allocate(lines(nlines),add_ampersand(nlines))

    ! Store all the file's lines into the lines array
    open(10, file=input_file(n), status='old', action='read')
    do i=1,nlines
       read(10,'(a)') lines(i)
    end do
    close(10)

    ! Check if the first character of a line is "&"
    ! If true, add an "&" at the end of the previous line
    add_ampersand(:) = .false.
    do i = 1, nlines
       if (len_trim(lines(i)) .ge. 6 .and. lines(i)(6:6) .eq. '&') then
          add_ampersand(i) = .true.
       end if
    end do

    ! Correct the ampersand location at the beginning and the end of a 
    ! break line.
    do i = 1, nlines
       if (add_ampersand(i)) then
          pos = INDEX(trim(lines(i-1)), '!')
          if(pos .gt. 0) then
            if(pos.eq.1) then
              lines(i-1) = trim(lines(i-1))//' &'
            else
              ltemp=trim(lines(i-1))
              lines(i-1) = trim(ltemp(1:pos-1))//' & '//ltemp(pos:)
            end if
          else
            lines(i-1) = trim(lines(i-1)) // ' &'
          end if
       end if
    end do
    
    ! Change the C and c comment 77 format into !
    do i = 1,nlines
      pos = INDEX(trim(lines(i)),'C')
      if(pos .eq. 1) then
        ltemp=trim(lines(i))
        lines(i) = '! '//ltemp(pos+1:)
      end if

      pos = INDEX(trim(lines(i)),'c')
      if(pos .eq. 1) then
        ltemp=trim(lines(i))
        lines(i) = '! '//ltemp(pos+1:)
      end if
    end do

    ! Harmonised the syntax of the fortran file:
     do i = 2,nlines
       pos2 = INDEX(trim(lines(i)),'!')


       pos = INDEX(trim(lines(i)),'integer ')
       if(pos.gt.0) then 
         ltemp=trim(lines(i))
         if(pos2.eq.0 .or. (pos2.gt.0 .and. pos2.gt.pos)) then
           lines(i) = '      INTEGER'//ltemp(pos+len('integer'):)
         else
           lines(i) = ltemp(:pos-1)//'INTEGER'//ltemp(pos+len('integer'):)
         end if
       end if

       pos = INDEX(trim(lines(i)),'double precision ')
       if(pos.gt.0) then
         ltemp=trim(lines(i))
         if(pos2.eq.0 .or. (pos2.gt.0 .and. pos2.gt.pos)) then
           lines(i) = '      DOUBLE PRECISION'//ltemp(pos+len('double precision'):)
         else
           lines(i) = ltemp(:pos-1)//'DOUBLE PRECISION'//ltemp(pos+len('double precision'):)
         end if
       end if

       pos = INDEX(trim(lines(i)),'real ')
       if(pos.gt.0) then
         ltemp=trim(lines(i))
         if(pos2.eq.0 .or. (pos2.gt.0 .and. pos2.gt.pos)) then
           lines(i) = '      REAL'//ltemp(pos+len('real'):)
         else
           lines(i) = ltemp(:pos-1)//'REAL'//ltemp(pos+len('real'):)
         end if
       end if

       pos = INDEX(trim(lines(i)),'character')
       if(pos.gt.0) then
         ltemp=trim(lines(i))
         if(pos2.eq.0 .or. (pos2.gt.0 .and. pos2.gt.pos)) then
           lines(i) = '       CHARACTER'//ltemp(pos+len('character'):)
         else
           lines(i) = ltemp(:pos-1)//'CHARACTER'//ltemp(pos+len('character'):)
         end if
       end if

       pos = INDEX(trim(lines(i)),'logical ')
       if(pos.gt.0) then
         ltemp=trim(lines(i))
         if(pos2.eq.0 .or. (pos2.gt.0 .and. pos2.gt.pos)) then
           lines(i) = '       LOGICAL'//ltemp(pos+len('logical'):)
         else
           lines(i) = ltemp(:pos-1)//'LOGICAL'//ltemp(pos+len('logical'):)
         end if
       end if

       pos = INDEX(trim(lines(i)),'end subroutine')
       if(pos.gt.0) then
         ltemp=trim(lines(i))
         lines(i) = ltemp(:pos-1)//'END SUBROUTINE'//ltemp(pos+len('end subroutine'):)
       end if

       pos = INDEX(trim(lines(i)),'subroutine')
       if(pos.gt.0) then
         ltemp=trim(lines(i))
         lines(i) = ltemp(:pos-1)//'SUBROUTINE'//ltemp(pos+len('subroutine'):)
       end if

       pos = INDEX(trim(lines(i)),'use')
       if(pos.gt.0) then
         ltemp=trim(lines(i))
         lines(i) = '       use'//ltemp(pos+len('use'):)
       end if

       pos = INDEX(trim(lines(i)),'module')
       if(pos.gt.0) then
         ltemp=trim(lines(i))
         lines(i) = '       module'//ltemp(pos+len('module'):)
       end if

       pos = INDEX(trim(lines(i)),'FI')
       if(pos.gt.0) then
         ltemp=trim(lines(i))
         lines(i) = ltemp(:pos-1)//'fi'//ltemp(pos+len('FI'):)
       end if
     end do
    
    ! Change to allocatable to local arrays:
    sn=len(trim(adjustl(input_file(n))))
    fn=input_file(n)
    if(fn(:6).ne.'module') then 
      do i = 1,nlines
      pos = INDEX(trim(lines(i)),'pointer')
      if(pos.gt.0) then
        pos1 = INDEX(trim(lines(i)),'fi')
        pos2 = INDEX(trim(lines(i)),'sbuf')
        pos3 = INDEX(trim(lines(i)),'rbuf')
        if(pos1 .eq.0 .and. pos2.eq.0 .and. pos3.eq.0) then
          ltemp=trim(lines(i))
          lines(i) = ltemp(:pos-1)//'allocatable'//ltemp(pos+len('pointer'):)
        end if
      end if
      end do
    end if

    ! Write the modified Fortran code to the output file
    sn=len(trim(adjustl(input_file(n))))
    fn=trim(adjustl(input_file(n)))
    output_file=trim(adjustl(fn(:sn-4)))//'.f90'
    open(20, file=output_file)
    do i = 1, nlines
       write(20, '(A)') trim(lines(i))
    end do
    close(20)

    ! Deallocate the dynamic array
    deallocate(lines,add_ampersand)

    write(6,'(a)') trim(adjustl(input_file(n)))//' changed to '//trim(adjustl(output_file))
  end do

  deallocate(input_file)

end program layout_change
