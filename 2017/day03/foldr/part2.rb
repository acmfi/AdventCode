class C
  attr_reader :x, :y
  
  def initialize(x,y)
    @x = x
    @y = y
  end
  
  def self.[](x,y)
    C.new x,y
  end

  def ==(o)
    x == o.x and y == o.y
  end

  alias eql? ==
        
  def next
    return C[1,0] if x == 0 and y == 0
    return C[x+1, y] if (x == y and x < 0) or (-x == y and y < 0)
    return C[x, y-1] if -x == y and x < 0
    return C[x-1, y] if x == y
    return C[x-1, y] if x.abs < y.abs and y > 0
    return C[x+1, y] if x.abs < y.abs and y < 0
    return C[x, y+1] if y.abs < x.abs and x > 0
    return C[x, y-1] if y.abs < x.abs and x < 0
  end

  def pred
    return [] if x == 0 and y == 0
    return [C[0,0]] if x == 1 and y == 0
    #return [C[x-1, y+1], C[x-1, y]] if x-1 == -y and y < 0
    return [C[x, y-1], C[x-1, y-1]] if x == y and x > 0
    return [C[x, y+1], C[x+1, y+1]] if x == y and x < 0
    return [C[x+1, y], C[x+1, y-1]] if -x == y and x < 0
    return [C[x, y+1], C[x-1, y+1], C[x-1, y]] if -x == y and x > 0
    return [C[x+1, y], C[x+1, y-1], C[x, y-1]] if x.abs < y.abs and y > 0
    return [C[x, y+1], C[x-1, y], C[x-1, y+1]] if x.abs < y.abs and y < 0
    return [C[x, y-1], C[x-1, y-1], C[x-1, y]] if y.abs < x.abs and x > 0
    return [C[x, y+1], C[x+1, y], C[x+1, y+1]] if y.abs < x.abs and x < 0
  end

  def neighbors
    [
      C[x, y+1], C[x-1, y+1], C[x+1, y+1],
      C[x-1, y], C[x+1, y],
      C[x, y-1], C[x-1, y-1], C[x+1, y-1]
    ]
  end

  def to_s
    "(#{x},#{y})"
  end

  def inspect
    to_s
  end

  def hash
    x.hash ^ y.hash + x.hash
  end
end

class Table
  attr_reader :last, :map
  
  def initialize(limit)
    @map = {
      C[0,0] => 1,
      C[1,0] => 1,
    }
    @limit = limit
    @last = 0
    @current = C[0,0]
  end

  def step
    if get(@current) > @limit
      @last = get(@current)
      nil
    else
      @current = @current.next
    end
  end

  def get(coord)
    @map[coord] || @map[coord] =
      coord.neighbors.select {|c| @map.key? c}.map {|c| get c}.sum
  end
end

table = Table.new 289326

while table.step;end

puts table.last
