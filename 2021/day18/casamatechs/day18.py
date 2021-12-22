from ast import literal_eval
from copy import deepcopy

f = open('input')

input = [literal_eval(line.strip('\n')) for line in f.readlines()]

class Node:
    def __init__(self,value:int=None,left=None,right=None):
        if value == None and (left == None or right == None):
            raise
        self.__value = value
        self.__left = left
        self.__right = right
    
    def __str__(self):
        if type(self.__value) == int:
            return self.__value
        return '[{},{}]'.format(self.__left.__str__(),self.__right.__str__())

    def explode(self):
        self.__left = None
        self.__right = None
        self.__value = 0
    
    def split(self):
        self.__left = Node(value=self.__value//2)
        self.__right = Node(value=self.__value - self.__left.__value)
        self.__value = None
    
    def calc_magnitude(self):
        if self.is_number():
            return self.__value
        else:
            return 3*self.__left.calc_magnitude() + 2*self.__right.calc_magnitude()
    
    def get_value(self):
        return self.__value

    def set_value(self, new_value):
        self.__value = new_value

    def get_left(self):
        return self.__left

    def get_right(self):
        return self.__right
    
    def set_left(self, node):
        self.__left = node

    def set_right(self, node):
        self.__right = node
    
    def is_number(self):
        return self.__left == None and self.__right == None and self.__value != None
    
    def is_pair(self):
        return not self.is_number()

def parse(number):
    if type(number) == int:
        return Node(value=number)
    return Node(left=parse(number[0]),right=parse(number[1]))

def explode(node: Node,nested_level=0,exploded=False,leaf:Node=None,value=0):
    if not exploded and nested_level >= 4 and node.is_pair():
        if leaf:
            leaf.set_value(leaf.get_value() + node.get_left().get_value())
        value = node.get_right().get_value()
        node.explode()
        return True,value,None
    if node.is_number():
        node.set_value(node.get_value() + value)
        return exploded,0,node
    exploded,value,leaf = explode(node.get_left(),nested_level+1,exploded,leaf,value)
    return explode(node.get_right(),nested_level+1,exploded,leaf,value)

def split_node(node:Node):
    if node.is_number():
        if node.get_value() > 9:
            node.split()
            return True
        return False
    ret = split_node(node.get_left())
    if ret:
        return ret
    ret = split_node(node.get_right())
    return ret

def reduce_snail(node:Node):
    reduce_actions = True
    while reduce_actions:
        if explode(node)[0]:
            continue
        elif split_node(node):
            continue
        else:
            reduce_actions = False
    return node

def sum_numbers(input: list):
    number_left = parse(input[0])
    for i in range(1,len(input)):
        number_left = reduce_snail(Node(left=deepcopy(number_left),right=parse(input[i])))
    return number_left

print(sum_numbers(input).calc_magnitude())

#Star 2

max_magnitude = 0
for left in input:
    for right in input:
        if left != right:
            max_magnitude = max(max_magnitude,reduce_snail(Node(left=parse(left),right=parse(right))).calc_magnitude())
print(max_magnitude)