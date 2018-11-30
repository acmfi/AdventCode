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

  def to_a
    @list
  end
end

class KnotHash
  def initialize(s)
    @lengths = s.split('').map(&:ord) + [17, 31, 73, 47, 23]
    @cl = CircularList.new
  end

  def sum
    sparse_hash
    dense_hash
  end

  private
  def sparse_hash
    64.times {
      @lengths.each { |l|
        @cl.reverse l
        @cl.advance l
      }
    }
  end

  def dense_hash
    h = []
    cl_a = @cl.to_a
    16.times { |i|
      j = i * 16
      h << cl_a[j..j+15].reduce(&:^)
    }
    h.map {|x| sprintf "%02x", x }.join('')
  end
end

input = "189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62"

puts (KnotHash.new input).sum

