# Convolution
Image Convolutions in Haskell

## Build
ghc -O convolve.hs

## Run
./convolve <filename> <kernel>

filename - input file - Greyscale 8 bits-per-pixel Windows bitmap
kernel   - 'blur', 'gblur', 'edge', 'sharpen' or 'emboss'

e.g. ./convolve lotus.bmp emboss
output file is lotus_emboss.bmp
