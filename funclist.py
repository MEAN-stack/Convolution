# Functional stuff with lists

def head(xs):
    if len(xs) == 0:
        raise Exception("Empty list")
    return xs[0]

def tail(xs):
    if len(xs) == 0:
        raise Exception("Empty list")
    return xs[1:]

take = lambda n: lambda xs: xs[:n]

drop = lambda n: lambda xs: xs[n:]

zipWith = lambda f: lambda xs: lambda ys: [ f(a)(b) for (a,b) in zip(xs,ys) ]

def transpose(xss):
    if len(xss[0])==1:
        return [list(map(head, xss))]
    else:
        return [list(map(head, xss))]+transpose(list(map(tail, xss)))

def split1 (n, xs):
    if len(xs) == 0:
        return []
    else:
        return [take(n)(xs)]+split1(n, drop(n)(xs))

split = lambda n: lambda xs: split1(n, xs)


mapmap = lambda f: lambda xss: [[*map(f,x)] for x in xss]

def concat(xss):
    return [inner for outer in xss for inner in outer]

if __name__ == "__main__":
    assert (head([2,3,4]) == 2)
    assert (head([[1,2,3],[2,3,4]]) == [1,2,3])

    assert (tail([2,3,4]) == [3,4])
    assert (tail([[1,2,3],[2,3,4]]) == [[2,3,4]])

    assert (take(0)([2,3,4,5,6]) == [])
    assert (take(3)([2,3,4,5,6]) == [2,3,4])
    assert (take(10)([2,3,4,5,6]) == [2,3,4,5,6])

    assert (drop(0)([2,3,4,5,6]) == [2,3,4,5,6])
    assert (drop(3)([2,3,4,5,6]) == [5,6])
    assert (drop(10)([2,3,4,5,6]) == [])

    assert (zipWith(lambda x: lambda y: x*y)([2,3,4])([5,6,7]) == [10,18,28])
    assert (zipWith(lambda x: lambda y: x+y)([2,3,4])([5,6,7]) == [7,9,11])

    xss = [[1,2,3],
           [4,5,6],
           [7,8,9]]

    xsst = [[1,4,7],
            [2,5,8],
            [3,6,9]]

    yss = [[10,11,12],
           [20,21,22],
           [30,31,32]]

    ysst = [[10,20,30],
            [11,21,31],
            [12,22,32]]

    zss = [[1,2],
           [3,4],
           [5,6],
           [7,8]]

    zsst = [[1,3,5,7],
            [2,4,6,8]]

    wss = [[1,2,3,4,5,6]]
    wsst = [[1],[2],[3],[4],[5],[6]]

    assert (zipWith(zipWith(lambda x: lambda y: x*y))(xss)(yss) == [[10, 22, 36], [80, 105, 132], [210, 248, 288]])

    assert (transpose(xss) == xsst)
    assert (transpose(yss) == ysst)
    assert (transpose(zss) == zsst)
    assert (transpose(wss) == wsst)

    assert (transpose(transpose(xss)) == xss)
    assert (transpose(transpose(yss)) == yss)
    assert (transpose(transpose(zss)) == zss)
    assert (transpose(transpose(wss)) == wss)

    assert(split(3)([1,2,3,4,5,6]) == [[1,2,3],[4,5,6]])

    assert (mapmap(lambda x: x*x)([[1,2,3],[4,5,6]]) == [[1, 4, 9], [16, 25, 36]])

    assert (concat(xss) == [1,2,3,4,5,6,7,8,9])