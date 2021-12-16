import math

f = open('input')

input = [line.strip('\n') for line in f.readlines()][0]

def get_binary_input(input):
    input_binary = ''
    for char in input:
        bin_char = bin(int(char,16))[2:]
        n0 = '0'*(4-len(bin_char))
        input_binary += n0 + bin(int(char,16))[2:]
    return input_binary

input_binary = get_binary_input(input)

def get_version_number(packet):
    return int(packet[:3],2)

def get_packet_type(packet):
    return int(packet[3:6],2)

def get_version_counter(input_binary,version_counter = 0, subpacket=False):
    i = 0
    while True:
        version_counter += get_version_number(input_binary[i:])
        packet_type = get_packet_type(input_binary[i:])
        if packet_type == 4:
            j = i + 6
            while input_binary[j] != '0':
                j += 5
            j += 5
            i = j
            if subpacket or len(input_binary[i:]) == 0 or int(input_binary[i:],2) == 0:
                break
        else:
            j = i + 6
            if input_binary[j] == '0':
                bits_length = 15
                j += 1
                total_length = int(input_binary[j:j+bits_length],2)
                j += bits_length
                version_counter,_ = get_version_counter(input_binary[j:j+total_length],version_counter)
                i = j + total_length
            else:
                bits_length = 11
                j += 1
                number_subpackets = int(input_binary[j:j+bits_length],2)
                j += bits_length
                for _ in range(number_subpackets):
                    version_counter, jj = get_version_counter(input_binary[j:],version_counter,subpacket=True)
                    j += jj
                i = j
            if subpacket or len(input_binary[i:]) == 0 or int(input_binary[i:],2) == 0:
                break
    return version_counter, i

def get_expression(input_binary,expression = [],subpacket=False):
    i = 0
    values = []
    while True:
        packet_type = get_packet_type(input_binary[i:])
        if packet_type == 4:
            j = i + 6
            lit = ''
            while input_binary[j] != '0':
                lit += input_binary[j+1:j+5]
                j += 5
            lit += input_binary[j+1:j+5]
            j += 5
            lit = int(lit,2)
            values.append(lit)
            i = j
            if subpacket or len(input_binary[i:]) == 0 or int(input_binary[i:],2) == 0:
                return expression + values,i
        else:
            if len(values) > 0:
                expression += values
                values = []
            j = i + 6
            if input_binary[j] == '0':
                bits_length = 15
                j += 1
                total_length = int(input_binary[j:j+bits_length],2)
                j += bits_length
                operator_values,_ = get_expression(input_binary[j:j+total_length],expression=[])
                i = j + total_length
            else:
                bits_length = 11
                j += 1
                number_subpackets = int(input_binary[j:j+bits_length],2)
                j += bits_length
                operator_values = []
                for _ in range(number_subpackets):
                    value, jj = get_expression(input_binary[j:],expression=[],subpacket=True)
                    operator_values.append(value[0])
                    j += jj
                i = j
            if packet_type == 0:
                expression.append(sum(operator_values))
            elif packet_type == 1:
                expression.append(math.prod(operator_values))
            elif packet_type == 2:
                expression.append(min(operator_values))
            elif packet_type == 3:
                expression.append(max(operator_values))
            elif packet_type == 5:
                expression.append(1 if operator_values[0] > operator_values[1] else 0)
            elif packet_type == 6:
                expression.append(1 if operator_values[0] < operator_values[1] else 0)
            elif packet_type == 7:
                expression.append(1 if operator_values[0] == operator_values[1] else 0)
            if subpacket or len(input_binary[i:]) == 0 or int(input_binary[i:],2) == 0:
                break
    return expression, i
print(get_version_counter(input_binary))
print(get_expression(input_binary))