# Fortran-ppm
Basic Fortran library for PPM image reading and writting. Accepts
grayscale and color images as well as up to a maximum of 16 bits
per pixel of color depth.

## Usage
The library consists of two functions, `ppmload` and `ppmwrite`. 
They both require the same number and kinds of arguments in the 
same order: 
- `filename`, a file name to open/read a ppm image file
- `im_ptr`, a rank-3 pointer to the image data with the shape `(nc, ny, nx)`
- `nc` number of color channels
- `ny` height of the image 
- `nx` width of the image
- `mxvl` an integer to store the maximum value of the image while
  reading and to indicate its maximum value while writting.

An example of both reading and writting images is contained in the
_tests_ folder.
