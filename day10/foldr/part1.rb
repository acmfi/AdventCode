class CircularList
  attr_reader :list, :skip_size
  
  def initialize(size=256)
    @list = (0..size-1).to_a
    @current_pos = 0
    @skip_size = 0
  end

  def [](i)
    @list[i % length]
  end

  def []=(i,e)
    @list[i % length] = e
  end

  def swap(i,j)
    self[i], self[j] = self[j], self[i]
  end

  def current
    self[@current_pos]
  end

  def reverse(n)
    b = @current_pos
    e = (@current_pos + n - 1)
    while e > b
      swap b, e
      b += 1
      e -= 1
    end
  end

  def advance(n)
    @current_pos += @skip_size + n
    @skip_size += 1
  end

  def length
    @list.length
  end

  def inspect
    to_s
  end

  def to_s
    @list.map.with_index {|x,i| (i == (@current_pos % length))? "(#{x})" : " #{x} " }.join ' '
  end

  def solve
    @list[0..1].reduce {|x,y| x*y}
  end
end

#lengths = [3, 4, 1, 5]
lengths = [189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62]

cl = CircularList.new
#puts "Initial\t\t[#{cl}]"
for l in lengths
  cl.reverse l
#  puts "Reverse(#{l})\t[#{cl}]"
  cl.advance l
#  puts "Advance(#{l}, #{cl.skip_size-1})\t[#{cl}]"
end
puts cl.solve
