module Main where

import System.Environment (getArgs)
import Data.List
import qualified Data.ByteString as BS

rounddiv p q = if (r*2) >= q then d+1 else d
               where d = p `div` q
                     r = p `mod` q

kernId           = [[0,0,0],[0,1,0],[0,0,0]]
kernBlur         = [[1,1,1],[1,1,1],[1,1,1]]
kernGaussianBlur = [[1,2,1],[2,4,2],[1,2,1]]
kernEdgeDetect   = [[-1,-1,-1],[-1,8,-1],[-1,-1,-1]]
kernSharpen      = [[0,-1,0],[-1,5,-1],[0,-1,0]]
kernEmboss       = [[-2,-1,0],[-1,1,1],[0,1,2]]

ksum = sum.map sum

-- image convolution with a given kernel
-- x is the neighbourhood of a pixel
-- x has the same size and shape as the kernel
--
convolve kernel w x = ((max 0).(min 255)) val
                      where val = ((`rounddiv` w) . ksum . kmul kernel) x
                            kmul = zipWith (zipWith (*))

-- convert a list into a list of 'neighbourhoods'
-- i.e. the elements to either side of each element in the original list
--
-- e.g. ns "Hello" -> ["Hel","ell","llo"]
-- e.g. ns [1,2,3,4,5,6] -> [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]
--
ns [] = []
ns (x:[]) = []
ns (x:y:[]) = []
ns z = take 3 z : ns (tail z)

-- split a list into a 2-D list of lists
-- e.g. split 2 "Hello!" -> ["He","ll","o!"]
-- e.g. split 3 [1,2,3,4,5,6] -> [[1,2,3],[4,5,6]]
--
split n [] = []
split n x = take n x : (split n . drop n) x

-- create a border around the edge of our image
--
addBorder xss = (extend . map extend) xss
                where extend = extendr.extendl
                      extendr = reverse.extendl.reverse
                      extendl (x:xs) = x:x:xs

convolveImage :: [[Integer]] -> [[Integer]] -> [[Integer]]
convolveImage img ker = (map.map) (convolve ker weight) ws
                        where ys = ns img
                              zs = (map.map) ns ys
                              ws = map transpose zs
                              w = ksum ker
                              weight = if w==0 then 1 else w

-- 32 bits little endian at offset 0x12
getBitmapHeaderWidth bs = b0+b1*256+b2*256*256+b3*256*256*256
                          where b0 = fromIntegral(BS.index bs 18)
                                b1 = fromIntegral(BS.index bs 19)
                                b2 = fromIntegral(BS.index bs 20)
                                b3 = fromIntegral(BS.index bs 21)

getKernel "blur"    = kernBlur
getKernel "gblur"   = kernGaussianBlur
getKernel "edge"    = kernEdgeDetect
getKernel "sharpen" = kernSharpen
getKernel "emboss"  = kernEmboss
getKernel _         = kernId

main :: IO ()
main = do
    args <- getArgs
    let inFile = head args
    contents <- BS.readFile inFile
    let kern = last args
    let header = BS.take 1078 contents
    let width = getBitmapHeaderWidth header
    let pixelData = BS.drop 1078 contents
    let imagePixels = (addBorder . split width . map toInteger . BS.unpack) pixelData
    let newImage = convolveImage imagePixels (getKernel kern)
    let bytes = BS.append header (BS.pack ((map fromInteger . concat) newImage))
    let outFile = takeWhile (/='.') inFile ++ '_':kern ++ (dropWhile (/='.') inFile)
    BS.writeFile outFile bytes
