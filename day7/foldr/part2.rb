require 'pp'

class N
  attr_reader :children, :name

  def initialize(name, weigth, children)
    @name = name
    @weigth = weigth
    @children = children
  end

  def self.[](n,w,c)
    N.new(n,w,c)
  end

  def to_s
    "(#{@name} #{@weigth} #{children.map(&:to_s).join(" ")})"
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

  def weigth
    children.select {|c| c.is_a? N}.map(&:weigth).sum + @weigth
  end

  def balanced?
    children.map(&:weigth).uniq.length <= 1
  end

  def self_weigth
    @weigth
  end

  def find_unbalanced
    unbalanced = children.reject(&:balanced?)
    if unbalanced.empty?
      if balanced?
        nil
      else
        ws = children.map {|c| [c.weigth, c.self_weigth]}
        ws.map {|w,sw| [w, sw, ws.map{|c|c[0]}.count(w)]}
      end
    else
      unbalanced.map(&:find_unbalanced)[0]
    end
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



tree = parse_input
#puts tree
p tree.find_unbalanced

  
