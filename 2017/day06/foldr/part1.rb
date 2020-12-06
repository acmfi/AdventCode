require 'set'

class Realloc
  attr_reader :steps
  
  def initialize(mem)
    @mem = mem
    @seen = Set.new
    @steps = 0
  end

  def step
    @steps += 1
    i = @mem.index(@mem.max)
    val = @mem[i]
    @mem[i] = 0
    val.times {
      i += 1
      @mem[i % @mem.length] += 1
    }
    if @seen.include? @mem
      nil
    else
      @seen << @mem
    end
  end

  def to_s
    @mem.join(' ')
  end
end

mem = gets.chomp.split(/\s+/).map(&:to_i)
realloc = Realloc.new mem
puts realloc while realloc.step

puts realloc.steps
