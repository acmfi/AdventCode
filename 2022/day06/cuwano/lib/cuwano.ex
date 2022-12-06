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
    String.graphemes(text)
  end

  def star1(input) do
    detect_marker(input, 4)
  end

  defp detect_marker([_ | rest] = input, result) do
    length = input |> Enum.take(4) |> Enum.uniq() |> length

    if length == 4 do
      result
    else
      detect_marker(rest, result + 1)
    end
  end

  def star2(input) do
    detect_marker_2(input, 14)
  end

  defp detect_marker_2([_ | rest] = input, result) do
    length = input |> Enum.take(14) |> Enum.uniq() |> length

    if length == 14 do
      result
    else
      detect_marker_2(rest, result + 1)
    end
  end
end
