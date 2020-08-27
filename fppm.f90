module fppm
    !use, intrinsic :: iso_c_binding, only: c_int8_t
    implicit none

    contains
        subroutine ppmload(filename, im_ptr, nc, ny, nx)
            character(len=*), intent(in)    :: filename
            integer, pointer, intent(inout) :: im_ptr(:, :, :)
            integer, intent(out)            :: nc
            integer, intent(out)            :: ny
            integer, intent(out)            :: nx

            character, parameter :: tab = char(9)
            character, parameter :: lf = char(10)
            character, parameter :: nl = char(12)
            character, parameter :: cr = char(13)
            character, parameter :: sp = char(32)

            character                :: byte
            character(len=2)         :: header
            integer                  :: exists, stat, i, j, k, mxvl
            integer(kind=1), pointer :: temp_ptr(:, :, :) => null() 
            character(len=6)         :: dims = "      "

            ! Checking existance of file
            exists = access(filename, "r")
            if (exists .ne. 0) then
                im_ptr => null() ! En cas que no existeixi
                return
            end if

            ! Reading file
            stat = 0 ! I/O status
            open(10, file=filename, access="stream")
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
                        read(10) byte
                        temp_ptr(k, j, i) = int(ichar(byte), kind=1)
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
            !im_ptr = reshape(transfer(temp_ptr, 1, size=nc*nx*ny), &
            !                 shape=[nc, ny, nx])

            ! Netejo
            deallocate(temp_ptr)

            return
        end subroutine ppmload

        subroutine ppm_write(file_name, im_ptr, nc, ny, nx)
            character(len=*), intent(in)    :: file_name
            integer, pointer, intent(in)    :: im_ptr(:, :, :)
            integer, intent(in)             :: nc
            integer, intent(in)             :: ny
            integer, intent(in)             :: nx
        end subroutine ppm_write
end module fppm
