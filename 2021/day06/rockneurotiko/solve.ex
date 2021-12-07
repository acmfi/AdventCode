defmodule Aoc.Aoc2021.Day06.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    input |> n_days(80) |> count_starfish()
  end

  defp count_starfish(state) do
    state |> Map.values() |> Enum.sum()
  end

  defp n_days(state, n) do
    Enum.reduce(1..n, state, fn _day, state ->
      day(state)
    end)
  end

  defp day(state) do
    {news, state} = Map.pop(state, 0, 0)

    state = reduce_day(state)

    state |> Map.put(8, news) |> Map.update(6, news, &(&1 + news))
  end

  defp reduce_day(state) do
    Map.new(state, fn {day, n} -> {day - 1, n} end)
  end

  def star2(input) do
    input |> n_days(256) |> count_starfish()
  end

  defp parse_line(line) do
    String.split(line, ",", trim: true)
    |> Enum.map(&String.to_integer/1)
    |> Enum.reduce(%{}, fn n, acc ->
      Map.update(acc, n, 1, &(&1 + 1))
    end)
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
