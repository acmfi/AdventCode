defmodule Aoc.Aoc2020.Day06.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    input |> Enum.map(&one_yes/1) |> Enum.sum()
  end

  def star2(input) do
    input |> Enum.map(&all_yes/1) |> Enum.sum() |> IO.inspect()
  end

  defp parse_line(line) do
    line |> String.split("\n", trim: true) |> Enum.map(&String.graphemes/1)
  end

  defp one_yes(group) do
    Enum.reduce(group, MapSet.new(), fn answer, map ->
      Enum.reduce(answer, map, fn x, map -> MapSet.put(map, x) end)
    end)
    |> MapSet.size()
  end

  defp all_yes(group) do
    {[first], other} = Enum.split(group, 1)

    Enum.reduce_while(other, MapSet.new(first), fn answer, map ->
      new_map = MapSet.intersection(map, MapSet.new(answer))

      case MapSet.size(new_map) do
        0 -> {:halt, new_map}
        _ -> {:cont, new_map}
      end
    end)
    |> MapSet.size()
  end

  # Helpers

  def solve(fname \\ "input") do
    do_solve(fname)
  end

  def solve_test(), do: solve("input_test")

  defp do_solve(fname) do
    path = Path.join(@base_path, fname)

    input = path |> File.read!() |> parse!()

    star1 = star1(input)
    IO.puts("Star 1: " <> to_string(star1))

    star2 = star2(input)
    IO.puts("Star 2: " <> to_string(star2))
  end

  defp parse!(t) do
    t
    |> String.split("\n\n", trim: true)
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&parse_line/1)
  end
end
