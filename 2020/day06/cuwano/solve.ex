defmodule AdventCode.Aoc2020.Day06.Solve do
  @base_path Path.dirname(__ENV__.file)

  def count_group_answers(group_answers) do
    group_answers
    |> Enum.join("")
    |> String.graphemes()
    |> MapSet.new()
    |> MapSet.to_list()
    |> length
  end

  def count_common_answers(group_answers) do
    group_answers
    |> Enum.map(&String.graphemes/1)
    |> Enum.map(&MapSet.new/1)
    |> Enum.reduce(fn x, acc -> MapSet.intersection(x, acc) end)
    |> MapSet.to_list()
    |> Enum.count()
  end

  def star1(input) do
    input |> Enum.map(&count_group_answers/1) |> Enum.sum()
  end

  def star2(input) do
    input |> Enum.map(&count_common_answers/1) |> Enum.sum()
  end

  defp parse_line(line) do
    line |> String.split("\n", trim: true)
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
    |> String.split("\n\n")
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&parse_line/1)
  end
end
