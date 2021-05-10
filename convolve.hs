module Main where

import Data.List
import qualified Data.ByteString as BS

-- choose a kernel
--
-- kernel = [[1,1,1],[1,1,1],[1,1,1]] -- blur
kernel = [[1,2,1],[2,4,2],[1,2,1]] -- Gaussian blur
-- kernel = [[-1,-1,-1],[-1,8,-1],[-1,-1,-1]] -- edge detection
-- kernel = [[0,-1,0],[-1,5,-1],[0,-1,0]] -- sharpen


-- image convolution with a given kernel
-- x is the neighbourhood of a pixel
-- x has the same size and shape as the kernel
-- 
convolve kernel x = if val<256 then val else 255
                    where val = ((`div` weight) . mxsum . mxmul kernel) x
                          mxmul = zipWith (zipWith (*))
                          mxsum = sum.map sum
                          w = mxsum kernel
                          weight = if w==0 then 1 else w

-- convert a list into a list of 'neighbourhoods'
-- i.e. the elements to either side of each element in the original list
--
-- e.g. ns "Hello" -> ["Hel","ell","llo"]
-- e.g. ns [1,2,3,4,5,6] -> [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]
--
ns (x:y:[]) = []
ns (x:[]) = []
ns [] = []
ns z = take 3 z : ns (tail z)

-- split a list into a 2-D list of lists
-- e.g. split 2 "Hello!" -> ["He","ll","o!"]
-- e.g. split 3 [1,2,3,4,5,6] -> 
--
split n [] = []
split n x = take n x : (split n . drop n) x

-- create a border of 0's around the edge our image
--
addBorder xss = replicate l z : yss ++ [replicate l z]
                where yss = map (\xs -> (z:xs)++[z]) xss
                      l = (length.head) yss
                      z = 0::Integer

convolveImage img ker = (map.map) (convolve ker) ws
                        where ys = ns img
                              zs = (map.map) ns ys
                              ws = map transpose zs

main :: IO ()
main = do
    contents <- BS.readFile "lena.bmp"
    let header = take 1078 (BS.unpack contents)
    let imagePixels = (addBorder . split 512 . map toInteger . drop 1078) (BS.unpack contents)
    let newImage = convolveImage imagePixels kernel
    let bytes = BS.pack (header++(map fromInteger . concat) newImage)
    BS.writeFile "lena_gblur.bmp" bytes
