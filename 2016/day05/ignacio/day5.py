import hashlib
import re

index = 0
door = 'ffykfhsq'

def get_hash(code):
    return hashlib.md5(str.encode(code)).hexdigest()

passwd = ''
while len(passwd) < 8:
    code = door + str(index)
    hs = get_hash(code)
    if hs.startswith('00000'): passwd += hs[5]
    index += 1

print('Part I \t-> h4X0r:',passwd)

# Part II
index = 0
passwd2 = ['','','','','','','','']
valid_digit = re.compile('00000([0-7])(.).*')
positions = []
while '' in passwd2:
    code = door + str(index)
    hs = get_hash(code)
    if valid_digit.match(hs) is not None:
        g = valid_digit.match(hs).groups()
        if g[0] not in positions:
            passwd2[int(g[0])] = g[1]
            positions.append(g[0])
            print('Positions: {}\tPasswd2: {}'.format("".join(positions),"".join(passwd2)))
    index += 1
    
print('Part II\t-> {}'.format("".join(passwd2)))
