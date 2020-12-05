require 'prime'

$generator = Prime::EratosthenesGenerator.new

$alpha = {}

puts "Starting initialization of lookup table"
for c in 'a'..'z'
  $alpha[c] = $generator.next
  print "."
end
puts "\nDone"

def prime_product(s)
  s.split('').map {|c| $alpha[c]}.reduce {|n,m| n*m}
end

def anagram?(a, b)
  prime_product(a) == prime_product(b)
end

count = 0
while line = gets
  line = line.chomp.split(' ')
  count += 1 if line.map {|x| if line.count(x) == 1 then true else false end}.reduce(&:&) and line.combination(2).map {|p| not anagram? p[0], p[1]}.reduce(&:&)
end
puts count
