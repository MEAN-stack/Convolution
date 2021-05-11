# Convolution
Image convolution in Haskell

## Build
ghc -O convolve.hs

## Run
./convolve <filename> <kernel>

filename - input file - Greyscale 8 bits-per-pixel Windows bitmap
kernel   - 'blur', 'gblur', 'edge', 'sharpen' or 'emboss'

E.g.:

<img src="lotus.bmp" alt="input" width="200"/>

`./convolve lotus.bmp emboss`


output file is lotus_emboss.bmp

<img src="lotus_emboss.bmp" alt="output" width="200"/>

## References
https://en.wikipedia.org/wiki/Kernel_(image_processing)

https://setosa.io/ev/image-kernels/
