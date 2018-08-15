# def check(das):
#     if das[0] > das[1]:
#         return [das[1], das[0]]
#     else:
#         return das
# 
# M=4; n=2
# data = [3,5,1,2,4,6,9,8,7]
# 
# left=[2,3]; right=[1,5]
# result = []
# 
# while(left and right):
#     print(left,right)
#     if left[0] > right[0]:
#         result.append(right[0])
#         del right[0]
#     else:
#         result.append(left[0])
#         del left[0]
# if left:
#     result += left
# else:
#     result += right
# print(result)

x = [42, "test", -12.34]
it = iter(x)
try:
    while True:
        x = next(it)
        print(x)
except StopIteration:
    pass
    
# %%
def genDict(raw1):
    data1 = [{'v':val, 'i':count+1} for (count,val) in enumerate(raw1)]
    return [list(a) for a in zip(*[iter(data1)] * 3)]
    
raw1 = [22,13,34,5,33,16,31,3,6]
raw2 = [4,5,7,3,2,5,6,4,2]
gr1 = genDict(raw1)
gr2 = genDict(raw2)

it1 = iter(gr1)
it2 = iter(gr2)
print(gr1)

# %%

dd = [[1,2,3],[4,5,6]]
def readNum(listing):
    for i in listing:
        yield i
def grep(pattern, lines):
    return (line for line in lines if pattern in line)
def printLine(lines):
    for line in lines:
        print('yes: ',line)

def main(pattern, fi):
    lines = readNum(fi)
    lines = grep(pattern, lines)
    printLine(lines)

main(4, dd)

# %%

data1 = [[1,2,3],[4,5,6],[2,5,7]]
data2 = [[11,22,3],[44,5,66],[4,2,7]]

# class getN:
#     def __init__(self, data1, data2):
#         self.data = data
#         self.i = 0
#     def __iter__(self):
#         return self
#     def __next__(self):
#         if not self.data:
import sys

class go:
    def __init__(self, dam1, dam2):
        self.dam2 = dam2
        self.it1 = iter(dam1)
        self.it2 = iter(dam2)
        self.d1 = next(self.it1)
    def __iter__(self):
        return self
    def __next__(self):
        d2 = next(self.it2, None)
        if not d2:
            self.d1 = next(self.it1, None)
            self.it2 = iter(self.dam2)
            d2 = next(self.it2, None)
        if not self.d1:
            print('ends')
            # sys.exit()
            raise StopIteration()
        # print(self.d1)
        # print(d2)
        for ii in self.d1:
            for jj in d2:
                if ii==jj and ii and jj:
                    return(ii,jj)
        

oo = go(data1, data2)
result = []
for i in oo:
    if i:
        result.append(i)
print(result)
