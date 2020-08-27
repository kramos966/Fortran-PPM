program test
    use fppm
    use, intrinsic :: iso_c_binding, only: c_int8_t
    implicit none
    character(len=*), parameter :: f_color = "testc.ppm"
    character(len=*), parameter :: f_grays = "test.ppm"
    integer, pointer :: im_ptr(:, :, :) => null()
    integer, pointer :: imc_ptr(:, :, :) => null()
    integer :: ny, nx, nc, mv1
    integer :: my, mx, mc, mv2
    integer :: err

    ! Reading test
    err = ppmload(f_grays, im_ptr, nc, ny, nx, mv1)
    write(*, fmt="(A)") "------------ Grayscale -------------"
    write(*, fmt="(A, L2)") "Associat: ", associated(im_ptr)
    write(*, fmt="(A8, I4)") "Width = ", nx
    write(*, fmt="(A8, I4)") "Height = ", ny
    write(*, fmt="(A8, I4)") "Colors = ", nc
    write(*, fmt="(A8, I4)") "Max = ", mv1
    write(*, fmt="(A, 20I3)") "Vint primers valors:", im_ptr(1, 1:20, 1)
    write(*, *)

    err = ppmload(f_color, imc_ptr, mc, my, mx, mv2)
    write(*, fmt="(A)") "------------ Color -------------"
    write(*, fmt="(A, L2)") "Associat: ", associated(imc_ptr)
    write(*, fmt="(A8, I4)") "Width = ", mx
    write(*, fmt="(A8, I4)") "Height = ", my
    write(*, fmt="(A8, I4)") "Colors = ", mc
    write(*, fmt="(A8, I4)") "Max = ", mv2
    write(*, fmt="(A, 20I3)") "Vint primers valors:", imc_ptr(1, 1:20, 1)

    ! Writing test
    err = ppmwrite("write.ppm", imc_ptr, mc, my, mx, mv2)
    write(*, *) err

end program test
