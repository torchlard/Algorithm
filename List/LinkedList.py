class Element():
    def __init__(self, val, ref=None):
        self.value = val
        self.ref = ref

class LinkedList():
    def __init__(self):
        self.head = None
        self.tail = None
    def insert(self,x):
        if self.head == None:
            self.head = Element(x)
            self.tail = self.head
        else:
            elem = Element(x)
            self.tail.ref = elem
            self.tail = elem
    def insertAll(self,x_list):
        for i in x_list:
            self.insert(i)
    def delete(self, target):
        current = self.head
        if current.ref != None:
            front = self.search_helper(target)
            if front==None:
                print(target,": No such element")
                return
            middle = front.ref
            if middle.ref:
                front.ref = middle.ref
            middle.ref = None
        elif current.value==target and current!=None:
            self.head = None
            self.tail = None
            
    def search(self, target):
        if self.search_helper(target)!=None:
            print("Value exist")
        else:
            print("not exist")
        
    def search_helper(self, target):
        current = self.head
        while (True) :
            if current.ref == None: # not found
                return None
            if (current.ref).value == target: # found
                return current 
            current = current.ref
            
    def printAll(self):
        x = self.head
        print_list = []
        while (True):
            print_list.append(x.value)
            if x.ref == None:
                break
            x = x.ref
        print(print_list)

data = [3,6,9,2,5,8,1]
result = LinkedList()
result.insertAll(data)
result.insert(4)
result.printAll()
result.delete(9)
result.insert(5)
result.insert(36)
result.delete(8)
result.printAll()
result.search(14)









