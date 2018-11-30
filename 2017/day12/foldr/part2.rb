class Vertex
  attr_accessor :id, :neighbors

  def initialize(id)
    @id = id
    @neighbors = []
  end
end

class Graph
  attr_accessor :vertices

  def initialize
    @vertices = []
  end

  def add_vertex(name)
    @vertices << Vertex.new(name)
  end

  def add_edge(from_id, to_id)
    from = vertices.index { |v| v.id == from_id }
    to = vertices.index { |v| v.id == to_id }
    vertices[from].neighbors[to] = true
    vertices[to].neighbors[from] = true
  end

  def has_vertex?(id)
    vertices.each do |v|
      return true if v.id == id
    end
    false
  end

  def [](id)
    vertices.each do |v|
      return v if v.id == id
    end
    nil
  end

  def connected_to(id)
    pending = [id]
    found = [id]
    while not pending.empty?
      i = pending.pop
      neighbors_of(i).map(&:id).each do |nid|
        unless found.include? nid
          found << nid
          pending << nid
        end
      end
    end
    found
  end

  def neighbors_of(id)
    idx = vertices.index {|v| v.id == id}
    neighbors = []
    vertices.length.times do |i|
      neighbors << vertices[i] if vertices[idx].neighbors[i]
    end
    neighbors
  end

  def ids
    @vertices.map(&:id)
  end
end

g = Graph.new
pipes = []
while line = gets
  pipes << line.chomp.split('<->').map(&:strip)
end
pipes.each {|id,_| g.add_vertex id}
pipes.each do |id, neighbors|
  neighbors.split(',').map(&:strip).each do |n|
    g.add_edge id, n
  end
end

groups = []

g.ids.each do |id|
  unless groups.any? {|grp| grp.include? id}
    groups << g.connected_to(id)
  end
end

puts groups.length
