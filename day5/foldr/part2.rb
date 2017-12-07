class MV
  attr_reader :pc, :mem, :steps

  def initialize(mem)
    @pc = 0
    @mem = mem
    @steps = 0
  end

  def step
    @steps += 1
    val = @mem[@pc]
    if val >= 3
      @mem[@pc] -= 1
    else
      @mem[@pc] += 1
    end
    @pc += val
    if @pc >= @mem.length
      nil
    else
      @pc
    end
  end

  def to_s
    @mem.each_with_index.map {|x,i| if i == @pc then "[#{x}]" else " #{x} " end}.join ' '
  end
end

mem = []
while line = gets
  mem << line.chomp.to_i
end

mv = MV.new mem
puts "#{mv.pc}/#{mv.mem.length}" while mv.step
#puts "#{mv}" while mv.step

puts mv.steps
