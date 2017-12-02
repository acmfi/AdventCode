checksum = 0
while line = gets
  line = line.chomp.split(' ').map(&:to_i)
  # Ugly as fuck, but is 6AM, so whatever man
  line.each_index do |i|
    line.each_index do |j|
      if i != j
        if line[i] % line[j] == 0
          checksum += line[i] / line[j]
        end
      end
    end
  end
end

puts checksum
