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
    text
    |> String.split("\n\n")
    |> Enum.map(fn pack ->
      pack |> String.split("\n") |> Enum.map(&String.to_integer/1)
    end)
  end

  def star1(input) do
    input
    |> Enum.map(&Enum.sum/1)
    |> Enum.max()
  end

  def star2(input) do
    input
    |> Enum.map(&Enum.sum/1)
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.sum()
  end
end
