defmodule Cuwano do
  def test(star) do
    parse_and_solve("test_input", star)
  end

  def solve(star) do
    parse_and_solve("input", star)
  end

  defp parse_and_solve(file_name, star) do
    solve = if star in [:star1, 1], do: &star1/1, else: &star2/1

    file_name |> parse_input() |> solve.() |> IO.inspect()
  end

  defp parse_input(file) do
    file |> File.read!() |> parse()
  end

  def parse(text) do
    text |> String.split("\n") |> Enum.map(&parse_ranges/1)
  end

  defp parse_ranges(text) do
    [range1, range2] = String.split(text, ",")
    {parse_range(range1), parse_range(range2)}
  end

  defp parse_range(text) do
    [starts, ends] = String.split(text, "-")

    starts = String.to_integer(starts)
    ends = String.to_integer(ends)

    starts..ends
  end

  def star1(input) do
    input
    |> Enum.map(&range_contains?/1)
    |> Enum.filter(& &1)
    |> Enum.count()
  end

  defp range_contains?({r1, r2}) do
    r1 = MapSet.new(r1)
    r2 = MapSet.new(r2)
    intersection = MapSet.intersection(r1, r2)

    intersection == r1 or intersection == r2
  end

  def star2(input) do
    input
    |> Enum.map(&range_overlaps?/1)
    |> Enum.filter(& &1)
    |> Enum.count()
  end

  defp range_overlaps?({r1, r2}) do
    r1 = MapSet.new(r1)
    r2 = MapSet.new(r2)
    intersection = MapSet.intersection(r1, r2)

    intersection != MapSet.new([])
  end
end
