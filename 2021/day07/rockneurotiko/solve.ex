defmodule Aoc.Aoc2021.Day07.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    find_best(input, &simple_fuel/2)
  end

  defp find_best(input, fuel_f) do
    min = Enum.min(input)
    max = Enum.max(input)

    Enum.reduce(min..max, 999_999_999_999_999, fn position, last_max ->
      moves = calc_moves(input, position, fuel_f)
      if moves < last_max, do: moves, else: last_max
    end)
  end

  defp calc_moves(input, position, fuel_f) do
    Enum.reduce(input, 0, &(fuel_f.(&1, position) + &2))
  end

  defp simple_fuel(original, next), do: abs(original - next)

  defp complex_fuel(original, next) do
    diff = abs(original - next)

    round(diff * ((diff + 1) / 2))
  end

  def star2(input) do
    find_best(input, &complex_fuel/2)
  end

  defp parse_line(line) do
    line |> String.split(",", trim: true) |> Enum.map(&String.to_integer/1)
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
    |> Enum.at(0)
  end
end
