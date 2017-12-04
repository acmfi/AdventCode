class PointsGenerator
  include Enumerable
  
  def initialize
    @limit = 1
    @x = 0
    @y = 0
    @g = []
  end
  
  def each
    loop do
      @x += 1
      unless @g.include? [@x, @y]
        @g << [@x, @y]
        yield [@x, @y]
      end
      for _ in 1..@limit
        @y -= 1
        unless @g.include? [@x, @y]
          @g << [@x, @y]
          yield [@x, @y]
        end
      end
      for _ in 0..@limit
        @x -= 1
        unless @g.include? [@x, @y]
          @g << [@x, @y]
          yield [@x, @y]
        end
      end
      for _ in 0..@limit
        @y += 1
        unless @g.include? [@x, @y]
          @g << [@x, @y]
          yield [@x, @y]
        end
      end
      for _ in 0..@limit
        @x += 1
        unless @g.include? [@x, @y]
          @g << [@x, @y]
          yield [@x, @y]
        end
      end
      @limit += 1
    end
  end
end

arr = { [0,0] => 1 }
last = [0,0]
input = gets.chomp.to_i

PointsGenerator.new.each do |x,y|
  arr[[x,y]] = [
    [x+1,y],
    [x-1,y],
    [x+1,y+1],
    [x+1,y-1],
    [x-1,y+1],
    [x-1,y-1],
    [x,y+1],
    [x,y-1]
  ].map {|p| arr[p]}.reject(&:nil?).sum
  last = [x,y]
  break if arr[[x,y]] > input
end

p last
puts arr[last]
