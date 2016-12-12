import hashlib

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

print('h4X0r:',passwd)
