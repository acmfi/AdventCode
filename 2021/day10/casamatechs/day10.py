f = open('input')
data = [line for line in f.readlines()]

inputs = [line.strip() for line in data]

closer_dict = {'{':'}','[':']','(':')','<':'>'}
illegal_chars = {')':0,'}':0,']':0,'>':0}
illegal_scores = {')':3,'}':1197,']':57,'>':25137}

lines_incomplete = []

for line in inputs:
    heap = []
    complete_line = True
    for char in line:
        if len(heap) == 0 or char in ['{','(','[','<']:
            heap += [char]
        else:
            if closer_dict[heap[-1]] == char:
                heap = heap[:-1]
            else:
                illegal_chars[char] += 1
                complete_line = False
                break
    if complete_line:
        lines_incomplete += [heap]

sum = 0
for k,v in illegal_chars.items():
    sum += v*illegal_scores[k]
print(sum)
closer_scores = {')':1,'}':3,']':2,'>':4}
scores = []
for line in lines_incomplete:
    closers = []
    for idx in range(len(line)-1, -1, -1):
        closers += [closer_dict[line[idx]]]
    score = 0
    for closer in closers:
        score = score * 5 + closer_scores[closer]
    scores += [score]
scores.sort()
print(scores[len(scores)//2])