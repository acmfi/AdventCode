format_r = /#(.+) @ (\d+),(\d+): (\d+)x(\d+)/

data = Hash.new 0

boxes = File.open("input").map do |line|
  id, left, top, wide, tall = line.match(format_r).captures
  left = left.to_i
  top = top.to_i
  wide = wide.to_i
  tall = tall.to_i

  right = left + wide - 1
  bottom = top + tall - 1

  (left..right).each do |i|
    (top..bottom).each do |j|
      coords = [i, j]
      data[coords] += 1
    end
  end


  [id, left, top, wide, tall]
end

puts "Solution 1:", data.values.select { |x| x > 1 }.count

def collides?(data, left, right, top, bottom)
  for i in (left..right) do
    for j in (top..bottom) do
      coords = [i, j]
      return false if data[coords] > 1
    end
  end

  true
end

one_remain = boxes.select do |box|
  _, left, top, wide, tall = box
  right = left + wide - 1
  bottom = top + tall - 1

  collides?(data, left, right, top, bottom)
end

puts "Solution 2:", one_remain[0][0]
