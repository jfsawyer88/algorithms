## Sorting algorithms
## for practice, posterity, and reference

## mergesort
## an application of the divide-and-conquer paradigm
def mergesort(arr):

    if len(arr) == 1:  # base case array has
        return arr     # only one value
    else:
        left  = mergesort(arr[:len(arr)/2]) # split list in
        right = mergesort(arr[len(arr)/2:]) # two and recurse

        i = 0
        j = 0
        out = []
        while i < len(left) and j < len(right): # merge the results from
            if left[i] < right[j]:              # the results above
                out.append(left[i])             # appending results from
                i += 1                          # left to right on each
            else:
                out.append(right[j])
                j += 1

        return out + left[i:] + right[j:]

## mergesort using stacks
## in the merge step
def mergesort2(arr):

    if len(arr) == 1:
        return arr

    left  = mergesort2(arr[:len(arr)/2])
    right = mergesort2(arr[len(arr)/2:])

    out = []
    while left and right:
        if left[-1] < right[-1]:
            out.append(left.pop())
        else:
            out.append(right.pop())

    return out + left + right

## mergesort using
## recursion in the
## merge setp
def merge(left, right):
    if len(left) == 0:
        return right
    if len(right) == 0:
        return left
    if left[0] < right[0]:
        return  [left[0]] + merge(left[1:], right)
    else:
        return [right[0]] + merge(left, right[1:])

#main body recursive mergesort
def mergesort3(arr):

    if len(arr) == 1:
        return arr

    left  = mergesort3(arr[:len(arr)/2])
    right = mergesort3(arr[len(arr)/2:])

    return merge(left, right)



#################################
# testing the code


if __name__ == '__main__':

    import timeit

    arr = [10, 2, 5, 3, 7, 13, 1, 6, 17, 11, 13, 4, 4, 2, 12, 14]
    print ''

    # ~30 micro seconds
    print timeit.timeit('mergesort(arr)',
                        'from __main__ import mergesort, arr',
                        number = 1000000)

    # ~41 micro seconds
    # ~22 micro seconds after fixing it
    # we can inspect the last element without poping it
    # this keeps up from poping and re-appending
    print timeit.timeit('mergesort2(arr)',
                        'from __main__ import mergesort2, arr',
                        number = 1000000)

    # ~34 micro seconds
    print timeit.timeit('mergesort3(arr)',
                        'from __main__ import mergesort3, arr',
                        number = 1000000)

    # <1 micro second
    # holy shit, this is so much faster
    print timeit.timeit('sorted(arr)',
                        'from __main__ import arr',
                        number = 1000000)
