defmodule Aoc.Aoc2020.Day03.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    slope = {3, 1}
    go_down(input, slope)
  end

  defp go_down(input, slope, trees \\ 0)
  defp go_down([], _slope, trees), do: trees

  defp go_down(input, {right, down}, trees) do
    input = input |> shift_left(right) |> shift_up(down)

    case input do
      [] -> trees
      [["#" | _] | _] -> go_down(input, {right, down}, trees + 1)
      _ -> go_down(input, {right, down}, trees)
    end
  end

  defp shift_left(input, 0), do: input
  defp shift_left(rows, n), do: shift_left(shift_all_rows(rows), n - 1)

  defp shift_all_rows(rows), do: Enum.map(rows, fn [x | xs] -> xs ++ [x] end)

  defp shift_up([], _), do: []
  defp shift_up(rows, 0), do: rows
  defp shift_up([_ | xs], n), do: shift_up(xs, n - 1)

  def star2(input) do
    slopes = [{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}]

    Enum.map(slopes, &go_down(input, &1)) |> Enum.reduce(&Kernel.*/2)
  end

  defp parse_line(line) do
    String.codepoints(line)
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
    |> String.split("\n")
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&parse_line/1)
  end
end
