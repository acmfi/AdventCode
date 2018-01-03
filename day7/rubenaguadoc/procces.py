import re


class procces(object):
    def __init__(self, rawData):
        name = re.search(r'.* \(', rawData)
        if name:
            self.name = name.group(0).replace(' (', '')

        weight = re.search(r'\(.*\)', rawData)
        if weight:
            self.weight = int(weight.group(0).replace('(', '').replace(')', ''))

        childs = re.search(r'-> .*', rawData)
        if childs:
            self.childs = childs.group(0).replace('-> ', '').split(', ')
        else:
            self.childs = []

    def calculateWeigth(self, proccesList):
        w = self.weight
        last = 0
        pre = ''
        for j in self.childs:
            for i in proccesList:
                if i.name == j:
                    new = i.calculateWeigth(proccesList)
                    if new != last and not last == 0:
                        print("Unbalance detected at node " + pre + " (" + str(last) + ") or node " + i.name + " (" + str(new) + ")")
                        print("Ctrl + F on the stack and modify the value of '" + pre + "' or '" + i.name + "' by " + str(abs(new - last)) + " to match the values and try again.")
                        exit()
                    else:
                        pre = i.name
                        last = new
                        w += new

        return w
