import numpy as np

f = open('input')
data = [line.rstrip() for line in f.readlines()]

matrix = np.zeros(shape=(len(data),len(data[0])), dtype=np.int32)
gamma_rate = ''
epsilon_rate = ''

for idx, row in enumerate(data):
    for jdx, n in enumerate(row):
        if n == '1':
            matrix[idx,jdx] = 1

for col in range(matrix.shape[1]):
    if np.sum(matrix[:,col]) > matrix.shape[0]/2:
        gamma_rate += '1'
        epsilon_rate += '0'
    else:
        gamma_rate += '0'
        epsilon_rate += '1'

print(int(gamma_rate,2)*int(epsilon_rate,2))

o2_matrix = np.copy(matrix)
co2_matrix = np.copy(matrix)

for col in range(o2_matrix.shape[1]):
    if np.sum(o2_matrix[:,col]) >= o2_matrix.shape[0]/2:
        o2_idx = [o2 for o2 in range(o2_matrix.shape[0]) if o2_matrix[o2,col] == 1]
        o2_idx_del = [o2 for o2 in range(o2_matrix.shape[0]) if o2 not in o2_idx]
        o2_matrix = np.delete(o2_matrix, o2_idx_del, 0)
    else:
        o2_idx = [o2 for o2 in range(o2_matrix.shape[0]) if o2_matrix[o2,col] == 0]
        o2_idx_del = [o2 for o2 in range(o2_matrix.shape[0]) if o2 not in o2_idx]
        o2_matrix = np.delete(o2_matrix, o2_idx_del, 0)
    if o2_matrix.shape[0] == 1:
        o2_rate = ''.join(map(str,o2_matrix[0]))
        break

for col in range(co2_matrix.shape[1]):
    if np.sum(co2_matrix[:,col]) < co2_matrix.shape[0]/2:
        co2_idx = [co2 for co2 in range(co2_matrix.shape[0]) if co2_matrix[co2,col] == 1]
        co2_idx_del = [co2 for co2 in range(co2_matrix.shape[0]) if co2 not in co2_idx]
        co2_matrix = np.delete(co2_matrix, co2_idx_del, 0)
    else:
        co2_idx = [co2 for co2 in range(co2_matrix.shape[0]) if co2_matrix[co2,col] == 0]
        co2_idx_del = [co2 for co2 in range(co2_matrix.shape[0]) if co2 not in co2_idx]
        co2_matrix = np.delete(co2_matrix, co2_idx_del, 0)
    if co2_matrix.shape[0] == 1:
        co2_rate = ''.join(map(str,co2_matrix[0]))
        break

print(int(o2_rate,2)*int(co2_rate,2))