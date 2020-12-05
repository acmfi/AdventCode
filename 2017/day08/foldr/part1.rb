class Reg
  include Comparable
  attr_reader :value

  def initialize(all_time_high_listener, value=0)
    @value = value
    @all_time_high_listener = all_time_high_listener
    @all_time_high = value
  end

  def inc(amount=1)
    @value += amount
    check_all_time_high
  end

  def dec(amount=1)
    @value -= amount
    check_all_time_high
  end

  def <=>(o)
    if o.is_a? Reg
      @value <=> o.value
    else
      @value <=> o
    end
  end

  def inspect
    @value.to_s
  end

  def to_s
    @value.to_s
  end

  private
  def check_all_time_high
    if @value > @all_time_high
      @all_time_high_listener.notify(@value) if @all_time_high_listener
      @all_time_high = @value
    end
  end
end

class VM
  attr_reader :all_time_high
  
  def initialize
    @regs = {}
    @pc = Reg.new(nil,0)
    @all_time_high = 0
  end

  def eval(stmt)
    print '.'
    reg($1).send($2, $3.to_i) if reg($4).send($5, $6.to_i) \
      if stmt =~ /([a-z]+) ([a-z]+) (-?\d+) if ([a-z]+) (.+) (-?\d+)/
    @pc.inc
  end

  def max
    @regs.map{|_,x|x}.max
  end

  def notify(value)
    if value > @all_time_high
      @all_time_high = value
    end
  end
  
  private
  def reg(name)
    @regs[name] || @regs[name] = Reg.new(self)
  end
  
end

vm = VM.new
while line = gets
  vm.eval line.chomp
end

puts "\n#{vm.all_time_high}"
