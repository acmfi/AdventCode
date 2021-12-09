defmodule Aoc.Aoc2021.Day09.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    input
    |> find_low_points()
    |> Enum.map(&(input[&1] + 1))
    |> Enum.sum()
  end

  defp find_low_points(input) do
    keys = Map.keys(input)

    {minx, miny} = Enum.min(keys)
    {maxx, maxy} = Enum.max(keys)

    for x <- minx..maxx, y <- miny..maxy, low_point?(input, x, y, maxx, maxy) do
      {x, y}
    end
  end

  defp low_point?(input, x, y, maxx, maxy) do
    neighs = neighs(x, y, maxx, maxy)

    elem = input[{x, y}]

    Enum.all?(neighs, fn npos ->
      elem < input[npos]
    end)
  end

  defp neighs({x, y}, maxx, maxy), do: neighs(x, y, maxx, maxy)

  defp neighs(ox, oy, maxx, maxy) do
    for x <- (ox - 1)..(ox + 1),
        y <- (oy - 1)..(oy + 1),
        x >= 0,
        y >= 0,
        x <= maxx,
        y <= maxy,
        {x, y} != {ox, oy},
        x == ox or y == oy do
      {x, y}
    end
  end

  def star2(input) do
    keys = Map.keys(input)
    {maxx, maxy} = Enum.max(keys)

    input
    |> find_low_points()
    |> Enum.map(&expand_basin(&1, input, maxx, maxy))
    |> Enum.map(&length/1)
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.reduce(&*/2)
  end

  defp expand_basin(low_point, input, maxx, maxy),
    do: expand_basin(low_point, input, maxx, maxy, [low_point])

  defp expand_basin(low_point, input, maxx, maxy, acc) do
    low_elem = input[low_point]

    ns =
      neighs(low_point, maxx, maxy)
      |> Enum.filter(fn neigh_pos ->
        neigh_elem = input[neigh_pos]
        neigh_elem != 9 and neigh_elem > low_elem and neigh_pos not in acc
      end)

    case ns do
      [] ->
        acc

      _ ->
        acc = acc ++ ns

        Enum.reduce(ns, acc, fn n, acc ->
          expand_basin(n, input, maxx, maxy, acc)
        end)
    end
  end

  defp parse_line(line) do
    String.graphemes(line) |> Enum.map(&String.to_integer/1)
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
    |> Enum.with_index()
    |> Enum.flat_map(fn {row, x} ->
      Enum.with_index(row) |> Enum.map(fn {elem, y} -> {{x, y}, elem} end)
    end)
    |> Map.new()
  end
end
