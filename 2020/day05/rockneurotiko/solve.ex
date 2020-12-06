defmodule Aoc.Aoc2020.Day05.Solve do
  @base_path Path.dirname(__ENV__.file)

  defmodule Reducer do
    def binary_reduce([x, y]), do: [x, y]

    def binary_reduce(x) do
      d = div(Enum.count(x), 2)
      x |> Enum.chunk_every(d) |> Enum.map(&binary_reduce/1)
    end
  end

  alias __MODULE__.Reducer

  @rows 0..127 |> Reducer.binary_reduce()

  @cols 0..7 |> Reducer.binary_reduce()

  @max_col 7

  def star1(input) do
    input |> Enum.map(&boarding_to_rc/1) |> Enum.map(&rc_to_seat_id/1) |> Enum.max()
  end

  def star2(input) do
    input
    |> Enum.map(&boarding_to_rc/1)
    |> Enum.map(&add_seat_id/1)
    |> Enum.sort_by(fn {_, y} -> y end)
    |> find_holes()
    |> Enum.map(&hole_to_next/1)
    |> Enum.at(0)
    |> elem(1)
  end

  defp find_holes(all, holes \\ [])
  defp find_holes([], holes), do: holes
  defp find_holes([_], holes), do: holes

  defp find_holes([{_, x}, {_, y} = rc | rest], holes) when x + 1 == y,
    do: find_holes([rc | rest], holes)

  defp find_holes([x, y | rest], holes), do: find_holes([y | rest], holes ++ [x])

  defp hole_to_next({{x, y}, _}) when y < @max_col, do: {{x, y + 1}, rc_to_seat_id({x, y + 1})}
  defp hole_to_next({{x, _}, _}), do: {{x + 1, 0}, rc_to_seat_id({x + 1, 0})}

  defp boarding_to_rc(board) do
    {rowsd, columnsd} = Enum.split(board, 7)

    row =
      Enum.reduce(rowsd, @rows, fn d, rows ->
        d |> where?() |> take(rows)
      end)

    column =
      Enum.reduce(columnsd, @cols, fn d, cols ->
        d |> where?() |> take(cols)
      end)

    {row, column}
  end

  defp add_seat_id(rc), do: {rc, rc_to_seat_id(rc)}

  defp rc_to_seat_id({r, c}), do: r * 8 + c

  defp take(:left, [l, _]), do: l

  defp take(:right, [_, r]), do: r

  defp where?("F"), do: :left
  defp where?("B"), do: :right
  defp where?("L"), do: :left
  defp where?("R"), do: :right

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
