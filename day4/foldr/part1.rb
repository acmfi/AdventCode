count = 0
while line = gets
  line = line.chomp.split(' ')
  count += 1 if line.map {|x| if line.count(x) == 1 then true else false end}.reduce(&:&)
end
puts count
