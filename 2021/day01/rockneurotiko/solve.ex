defmodule Aoc.Aoc2021.Day01.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    [first | rest] = input

    {_, s} =
      Enum.reduce(rest, {first, 0}, fn current, {prev, s} ->
        if current > prev do
          {current, s + 1}
        else
          {current, s}
        end
      end)

    s
  end

  def star2(input) do
    first = Enum.take(input, 3)
    rest = Enum.drop(input, 3)

    {_, s} =
      Enum.reduce(rest, {first, 0}, fn current, {prev, s} ->
        current = Enum.drop(prev, 1) ++ [current]

        if Enum.sum(current) > Enum.sum(prev) do
          {current, s + 1}
        else
          {current, s}
        end
      end)

    s
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
  end
end
