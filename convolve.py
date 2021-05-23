import sys
import argparse
from funclist import head, tail, take, drop, transpose, zipWith, mapmap, concat

kernId           = [[0,0,0],[0,1,0],[0,0,0]]
kernBlur         = [[1,1,1],[1,1,1],[1,1,1]]
kernGaussianBlur = [[1,2,1],[2,4,2],[1,2,1]]
kernEdgeDetect   = [[-1,-1,-1],[-1,8,-1],[-1,-1,-1]]
kernSharpen      = [[0,-1,0],[-1,5,-1],[0,-1,0]]
kernEmboss       = [[-2,-1,0],[-1,1,1],[0,1,2]]

# image convolution with a given kernel
# x is the neighbourhood of a pixel
# x has the same size and shape as the kernel
#
mul = lambda x: lambda y: x*y

def ksum(xss):
    return sum(list(map(sum, xss)))

def convolve1(kernel, weight, x):
    kmul = zipWith(zipWith(mul))
    val = ksum(kmul(kernel)(x)) // weight
    return max(0, min(255, val))

convolve = lambda k: lambda w: lambda x: convolve1(k, w, x)

# convert a list into a list of 'neighbourhoods'
# i.e. the elements to either side of each element in the original list
#
# e.g. ns "Hello" -> ["Hel","ell","llo"]
# e.g. ns [1,2,3,4,5,6] -> [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]
#
def ns(xs):
    if len(xs)<3:
        return []
    else:
        return [take(3)(xs)]+ns(tail(xs))

# split a list into a 2-D list of lists
# e.g. split 2 "Hello!" -> ["He","ll","o!"]
# e.g. split 3 [1,2,3,4,5,6] -> [[1,2,3],[4,5,6]]
#
def split1 (n, xs):
    if len(xs) == 0:
        return []
    else:
        return [take(n)(xs)]+split1(n, drop(n)(xs))

split = lambda n: lambda xs: split1(n, xs)

# create a border of 0's around the edge our image
#
def addBorder(xss):
    yss = list(map(lambda xs: [0]+xs+[0], xss))
    l = len(yss[0])
    return [l*[0]] + yss + [l*[0]]

def convolveImage(img, ker):
    w = ksum(ker)
    if w == 0:
        w = 1
    ys = ns(img)
    zs = mapmap(ns)(ys)
    ws = list(map(transpose, zs))
    return mapmap(convolve(ker)(w))(ws)

# 32 bits little endian at offset 0x12
def getBitmapHeaderWidth(bs):
    b0 = bs[18]
    b1 = bs[19]
    b2 = bs[20]
    b3 = bs[21]
    return b0+b1*256+b2*256*256+b3*256*256*256

def getKernel(kern):
    if kern == "blur":
        return kernBlur
    if kern == "gblur":
        return kernGaussianBlur
    if kern == "edge":
        return kernEdgeDetect
    if kern == "sharpen":
        return kernSharpen
    if kern == "emboss":
        return kernEmboss
    return kernId

def parse_command_line():
    parser = argparse.ArgumentParser(description="Convolution")
    parser.add_argument("-f", metavar="<filename>", help="path/filename of image")
    parser.add_argument("-k", metavar="<kernel>", help="kernel: edge, blur, emboss, etc.")
    args = parser.parse_args()
    return args

def main():
    sys.setrecursionlimit(3000)
    args = parse_command_line()
    inFile = args.f
    try:
        with open(inFile, 'rb') as file:
            contents = file.read()

    except (FileNotFoundError, PermissionError) as err:
        sys.stderr.write(str(err) + "\n")
        sys.stderr.write("Could not open " + inFile + "\n")

    except IOError as err:
        sys.stderr.write(str(err) + "\n")
        sys.stderr.write("Error reading " + inFile + "\n")

    kern = args.k
    header = take(1078)(contents)
    width = getBitmapHeaderWidth(header)
    pixelData = drop(1078)(contents)
    imagePixels = addBorder(split(width)(list(pixelData)))
    newImage = convolveImage(imagePixels, getKernel(kern))
    imagebytes = header + bytearray(concat(newImage))
    #let outFile = takeWhile (/='.') inFile ++ '_':kern ++ (dropWhile (/='.') inFile)
    outFile = open('outfile.bmp', 'wb')
    outFile.write(imagebytes)
    outFile.close()

if __name__ == "__main__":
    main()