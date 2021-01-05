defmodule Aoc.Aoc2020.Day10.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    d = find_differences(input)
    d[1] * d[3]
  end

  def star2(input) do
    arranges(input)
  end

  defp arranges(input) do
    Enum.reduce(input, {%{0 => 1}, 1}, fn i, {acc, m} ->
      s = Map.get(acc, i - 3, 0) + Map.get(acc, i - 2, 0) + Map.get(acc, i - 1, 0)

      {Map.put(acc, i, s), Enum.max([m, s])}
    end)
    |> elem(1)
  end

  defp find_differences(input, prev \\ 0, diffs \\ %{3 => 1})

  defp find_differences([], _prev, diffs), do: diffs

  defp find_differences([e | elems], prev, diffs) when e - 3 <= prev do
    d = e - prev
    diffs = Map.update(diffs, d, 1, &(&1 + 1))
    find_differences(elems, e, diffs)
  end

  defp parse_line(line) do
    String.to_integer(line)
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
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
    |> Enum.sort()
  end
end
