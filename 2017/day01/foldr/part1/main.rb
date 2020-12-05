numbers = gets.chomp.split('').map(&:to_i)
acc = 0
numbers.each_index do |i|
  j = (i+1) % numbers.length

  acc += numbers[i] if numbers[i] == numbers[j]
end
puts acc
