checksum = 0
while line = gets
  line = line.chomp.split(' ').map(&:to_i)
  checksum += line.max - line.min
end
puts checksum

