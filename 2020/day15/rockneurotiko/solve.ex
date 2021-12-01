defmodule Aoc.Aoc2020.Day15.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    find_nth(input, 2020)
  end

  defp find_nth(input, max) do
    st = Map.new(Enum.with_index(input), fn {x, i} -> {x, [i + 1]} end)
    i = length(input)
    last = Enum.at(input, -1)

    find_nth(i + 1, last, st, max)
  end

  defp find_nth(i, last, st, i) do
    {last, _st} = next_number(last, st, i)
    last
  end

  defp find_nth(i, last, st, max) do
    {last, st} = next_number(last, st, i)

    find_nth(i + 1, last, st, max)
  end

  defp next_number(last, st, i) do
    case Map.fetch(st, last) do
      {:ok, [_]} -> {0, add_n(st, 0, i)}
      {:ok, [x, y]} -> {y - x, add_n(st, y - x, i)}
    end
  end

  defp add_n(st, n, i) do
    case Map.fetch(st, n) do
      {:ok, [x]} -> Map.put(st, n, [x, i])
      {:ok, [_x, y]} -> Map.put(st, n, [y, i])
      _ -> Map.put(st, n, [i])
    end
  end

  def star2(input) do
    find_nth(input, 30_000_000)
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
    |> String.split(",", trim: true)
    |> Enum.map(&String.to_integer/1)
  end
end
