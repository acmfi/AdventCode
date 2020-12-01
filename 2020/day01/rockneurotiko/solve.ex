defmodule Aoc.Aoc2020.Day01.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    find_2020(input)
  end

  defp find_2020(input) do
    Stream.flat_map(input, fn x ->
      Stream.map(input, fn y ->
        {x, y}
      end)
      |> Stream.filter(fn {x, y} -> x + y == 2020 end)
      |> Stream.map(fn {x, y} -> x * y end)
    end)
    |> Enum.take(1)
    |> Enum.at(0)
  end

  def star2(input) do
    find3_2020(input)
  end

  defp find3_2020(input) do
    Stream.flat_map(input, fn x ->
      Stream.flat_map(input, fn y ->
        Stream.map(input, fn z ->
          {x, y, z}
        end)
        |> Stream.filter(fn {x, y, z} -> x + y + z == 2020 end)
        |> Stream.map(fn {x, y, z} -> x * y * z end)
      end)
    end)
    |> Enum.take(1)
    |> Enum.at(0)
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
    |> String.split("\n")
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&parse_line/1)
  end
end
