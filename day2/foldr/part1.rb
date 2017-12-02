checksum = 0
while line = gets
  line = line.chomp.split(' ').map(&:to_i)
  min = line.min
  max = line.max
  checksum += max - min
end

puts checksum

