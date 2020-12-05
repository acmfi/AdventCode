numbers = gets.chomp.split('').map(&:to_i)
acc = 0
numbers.each_index do |i|
  j = (i+numbers.length/2) % numbers.length

  acc += numbers[i] if numbers[i] == numbers[j]
end
puts acc
