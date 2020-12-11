defmodule AdventCode.Aoc2020.Day05.Solve do
  @base_path Path.dirname(__ENV__.file)

  def code_to_binary("F"), do: 0
  def code_to_binary("L"), do: 0
  def code_to_binary("B"), do: 1
  def code_to_binary("R"), do: 1

  def get_seat_id(seat) do
    seat |> String.graphemes() |> Enum.map(&code_to_binary/1) |> Integer.undigits(2)
  end

  def star1(input) do
    input |> Enum.map(&get_seat_id/1) |> Enum.max()
  end

  def star2(input) do
    next =
      input
      |> Enum.map(&get_seat_id/1)
      |> Enum.sort()
      |> Enum.reduce({false, 0}, fn x, {found, previous} ->
        if found do
          {found, previous}
        else
          {x - previous == 2, x}
        end
      end)
      |> elem(1)

    next - 1
  end

  defp parse_line(line) do
    line
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
