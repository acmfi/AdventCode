require 'set'

class Snapshot
  attr_reader :time, :mem

  def initialize(time, mem)
    @time = time
    @mem = mem
  end
end
    

class Realloc
  attr_reader :steps
  attr_reader :diff
  
  def initialize(mem)
    @mem = mem
    @seen = []
    @steps = 0
    @diff = 0
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
    if @seen.map(&:mem).include? @mem
      @diff = @steps - @seen.select {|s| s.mem == @mem}[0].time
      nil
    else
      @seen << Snapshot.new(@steps, @mem.clone)
    end
  end

  def to_s
    @mem.join(' ')
  end
end


mem = gets.chomp.split(/\s+/).map(&:to_i)
realloc = Realloc.new mem
puts "mem=#{realloc}" while realloc.step

puts realloc.diff
