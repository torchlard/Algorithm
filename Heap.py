class Tree():
    def __init__(self, value):
        self.value = value
        self.s_value = 0
        # reference to left, right tree
        self.left = None 
        self.right = None

class Heap():
    def __init__(self):
        self.head = None
    def add(self,x):
        if self.head == None:
            self.head = Tree(x)
        else:
            
    def compare(self,x,node):
        if x > node.value:
            old = node.value
            node.value = x
            self.compare(old,node)
            return
        if node.left!=None:
            if x > node.left:
    
    def swap(self, old,new):
        old_value = old.value
        old.value = new
        # if can still go deep
        if (old.left!=None or old.right!=None):
            self.compare(old_value, old)
        else:
            (old.new).left = old_value


# %%
import random
print([random.randint(0,100) for _ in range(20)])

[64, 45, 63, 1, 67, 49, 37, 95, 10, 94, 4, 75, 20, 30, 31, 100, 93, 33, 26, 59]


