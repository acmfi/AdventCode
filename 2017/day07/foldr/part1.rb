require 'pp'

class N
  attr_reader :weigth, :children, :name

  def initialize(name, weigth, children)
    @name = name
    @weigth = weigth
    @children = children
  end

  def self.[](n,w,c)
    N.new(n,w,c)
  end

  def to_s
    "(#{@name} #{weigth} #{children.map(&:to_s).join(" ")})"
  end

  def children?(m)
    r = false
    for c in @children
      if c.is_a? N
        r ||= c.children? m
      else
        r ||= c == m.name
      end
    end
    r
  end

  def insert(m)
    children.map! do |c|
      if c.is_a? N and c.children? m
        c.insert m
      elsif c == m.name
        m
      else
        c
      end
    end
    self
  end
end

def parse_input
  tree = []
  while line = gets
    line.chomp!
    if line =~ /^([a-z]+) \((\d+)\)( -> (([a-z]+, )*[a-z]+))?$/
      n = N[$1, $2.to_i, ($4 || "").split(', ')]
      ms = tree.select {|m| m.children? n}
      unless ms.empty?
        ms[0].insert n
      else
        tree << n
      end
    end
  end
  while tree.length != 1
    n = tree.delete_at 0
    ms = tree.select {|m| m.children? n}
    unless ms.empty?
      ms[0].insert n
    else
      tree << n
    end
  end
  tree[0]
end
puts parse_input.name

  
