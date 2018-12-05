defmodule Solve1.Agent do
  use Agent

  def start_link() do
    Agent.start_link(fn -> Matrix.new(1000, 1000) end, name: __MODULE__)
  end

  def fill_matrix(id: id, position: {px, py}, size: {sx, sy}) do
  end
end
