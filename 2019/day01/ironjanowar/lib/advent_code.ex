defmodule AdventCode do
  def get_input() do
    File.stream!("input")
  end

  def day_1() do
    get_input()
    |> Stream.map(&operate/1)
    |> Enum.reduce(0, fn x, acc -> x + acc end)
  end

  def operate(number) do
    fuel = number |> Integer.parse() |> elem(0) |> Integer.floor_div(3)
    fuel - 2
  end
end
