module fppm
    !use, intrinsic :: iso_c_binding, only: c_int8_t
    implicit none
    private

    character, parameter :: tab = char(9)
    character, parameter :: lf = char(10)
    character, parameter :: nl = char(12)
    character, parameter :: cr = char(13)
    character, parameter :: sp = char(32)
    integer, parameter   :: success = 0
    integer, parameter   :: error = -1

    public :: ppmload
    public :: ppmwrite
    contains
        function ppmload(filename, im_ptr, nc, ny, nx, mxvl) result(err)
            ! Reads a ppm file and allocates im_ptr to store it
            ! in memory. Returns the number of color channels, 
            ! the height ny, the width nx and the maximum value
            ! of the image mxvl.
            character(len=*), intent(in)    :: filename
            integer, pointer, intent(inout) :: im_ptr(:, :, :)
            integer, intent(out)            :: nc
            integer, intent(out)            :: ny
            integer, intent(out)            :: nx
            integer, intent(out)            :: mxvl
            integer                         :: err

            character                :: byte
            character(len=2)         :: header
            integer                  :: stat, i, j, k
            integer(kind=1), pointer :: temp_ptr(:, :, :) => null() 
            character(len=6)         :: dims = "      "

            ! Checking existance of file
            if (access(filename, "r") .ne. 0) then
                err = error
                return 
            end if

            ! Reading file
            stat = 0 ! I/O status
            open(10, file=filename, access="stream", action="read")
            read(10, iostat=stat) header
            ! Check if file is ppm
            if (header .eq. "P5") then
                nc = 1 ! Grayscale
            else
                if (header .eq. "P6") then
                    nc = 3 ! Color
                else
                    return ! Not a ppm
                end if
            end if


            ! Read image width
            ! FIXME: TERRIBLE HACK INCOMING
            i = 1
            read(10) byte
            byte = "a" ! Skip space
            do while ((byte .ne. tab) .and. &
                      (byte .ne. lf) .and. &
                      (byte .ne. cr) .and. &
                      (byte .ne. sp) .and. &
                      (byte .ne. nl))
                  read(10, iostat=stat) byte
                  dims(i:i) = byte
                  i = i+1
            end do
            read(dims(1:i-2), fmt="(I6)") nx
            ! Read image height
            dims = "      " ! Reinicio dims
            i = 1
            byte = "a"
            do while ((byte .ne. tab) .and. &
                      (byte .ne. lf) .and. &
                      (byte .ne. cr) .and. &
                      (byte .ne. sp) .and. &
                      (byte .ne. nl))
                read(10, iostat=stat) byte
                dims(i:i) = byte
                i = i+1
            end do
            read(dims(1:i-2), fmt="(I6)") ny ! Llegeix un caracter menys

            ! Read max value
            dims = "      "
            byte = "a"
            i = 1
            do while ((byte .ne. tab) .and. &
                      (byte .ne. lf) .and. &
                      (byte .ne. cr) .and. &
                      (byte .ne. sp) .and. &
                      (byte .ne. nl))
                read(10) byte
                dims(i:i) = byte
                i = i+1
            end do
            read(dims(1:i-2), fmt="(I6)") mxvl ! Idem

            ! Llegeix cada fila de dades, l'espai ja ha estat llegit
            if (mxvl .gt. 255) then
                allocate(temp_ptr(2*nc, ny, nx)) ! Cada pixel son dos bytes
            else
                allocate(temp_ptr(nc, ny, nx))   ! Cada pixel es un byte
            end if
            allocate(im_ptr(nc, ny, nx))    ! Creo espai per l'arr final
            do j = 1, ny
                do i = 1, nx
                    do k = 1, nc
                        !read(10) byte
                        read(10) temp_ptr(k, j, i)
                    end do
               end do
            end do

            ! Conversio a INTEGER*4 independentment dels bytes d'entrada
            if (mxvl .le. 255) then
                do concurrent (i = 1:nx)
                    do j = 1, ny
                        do k = 1, nc
                            im_ptr(k, j, i) = transfer(temp_ptr(k, j, i), &
                                                       int(1, kind=4))
                        end do
                    end do
                end do
            else
                do concurrent (i = 1:nx)
                    do j = 1, ny
                        do k = 1, nc
                            im_ptr(k, j, i) = &
                                transfer(temp_ptr(2*k-1:2*k, j, i), &
                                                       int(1, kind=4))
                        end do
                    end do
                end do

            end if

            ! Cleanup
            deallocate(temp_ptr)
            close(10)
            err = success
            return
        end function ppmload

        function ppmwrite(filename, im_ptr, nc, ny, nx, mxvl) result(err)
            ! Saves a 4byte integer array into a ppm file.
            character(len=*), intent(in)    :: filename
            integer, pointer, intent(in)    :: im_ptr(:, :, :)
            integer, intent(in)             :: nc
            integer, intent(in)             :: ny
            integer, intent(in)             :: nx
            integer, intent(in)             :: mxvl
            integer                         :: err

            character(len=50) :: header
            character(len=2)  :: magic
            character(len=10) :: width
            character(len=10) :: height
            character(len=6)  :: max_val
            integer           :: i, j, k

            ! Construction of the header of the ppm
            if (nc .eq. 1) magic = "P5"
            if (nc .eq. 3) then
                magic = "P6"
            else
                err = error
                return
            end if
            ! Width and height, terrible hacks included!
            write(width, fmt="(I10)") nx
            width = adjustl(width)
            write(height, fmt="(I10)") ny
            height = adjustl(height)
            write(max_val, fmt="(I6)") mxvl
            max_val = adjustl(max_val)

            header = trim(magic)//nl//trim(width)//sp//trim(height)//&
                nl//trim(max_val)//nl

            open(10, file=filename, access="stream", action="write")
            write(10) trim(header)
            do concurrent (j = 1:ny)
                do i = 1, nx
                    do k = 1, nc
                        write(10) int(im_ptr(k, j, i), kind=1)
                    end do
                end do
            end do
            close(10)

            err = success
            return
        end function ppmwrite
end module fppm
