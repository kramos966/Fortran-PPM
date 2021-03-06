program test
    use fppm
    use, intrinsic :: iso_c_binding, only: c_int8_t
    implicit none
    character(len=*), parameter :: f_color = "testc.ppm"
    character(len=*), parameter :: f_grays = "test.ppm"
    integer, allocatable :: im_arr(:, :, :)
    integer, allocatable :: imc_arr(:, :, :)
    integer, allocatable :: nim_arr(:, :, :)
    integer :: ny, nx, nc, mv1
    integer :: my, mx, mc, mv2
    integer :: err, i, j

    ! Reading test
    err = ppmload(f_grays, im_arr, nc, ny, nx, mv1)
    write(*, fmt="(A)") "------------ Grayscale -------------"
    write(*, fmt="(A8, I4)") "Width = ", nx
    write(*, fmt="(A8, I4)") "Height = ", ny
    write(*, fmt="(A8, I4)") "Colors = ", nc
    write(*, fmt="(A8, I4)") "Max = ", mv1
    write(*, fmt="(A, 20I3)") "Vint primers valors:", im_arr(1, 1:20, 1)
    write(*, *)

    err = ppmload(f_color, imc_arr, mc, my, mx, mv2)
    write(*, fmt="(A)") "------------ Color -------------"
    write(*, fmt="(A8, I4)") "Width = ", mx
    write(*, fmt="(A8, I4)") "Height = ", my
    write(*, fmt="(A8, I4)") "Colors = ", mc
    write(*, fmt="(A8, I4)") "Max = ", mv2
    write(*, fmt="(A, 20I3)") "Vint primers valors:", imc_arr(1, 1:20, 1)

    ! Writing test
    allocate(nim_arr(1, 256, 256))
    do concurrent (i = 1:256)
        do j = 1, 256
            nim_arr(1, j, i) = ieor(j-1, i-1)
        end do
    end do
    err = ppmwrite("write.ppm", nim_arr, 1, 256, 256, 255)
    write(*, *) err

end program test
