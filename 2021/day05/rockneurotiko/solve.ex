defmodule Aoc.Aoc2021.Day05.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    input
    |> Enum.filter(&non_diagonal?/1)
    |> Enum.reduce(%{}, &build_map/2)
    |> more_than_two()
  end

  defp non_diagonal?({left, right}) do
    non_diagonal?(left, right)
  end

  defp non_diagonal?({x, _}, {x, _}), do: true
  defp non_diagonal?({_, y}, {_, y}), do: true
  defp non_diagonal?(_, _), do: false

  defp build_map({left, right}, map) do
    line = build_line(left, right)

    Enum.reduce(line, map, fn point, map ->
      Map.update(map, point, 1, &(&1 + 1))
    end)
  end

  defp build_line({x, y1}, {x, y2}) do
    y1..y2 |> Enum.map(&{x, &1})
  end

  defp build_line({x1, y}, {x2, y}) do
    x1..x2 |> Enum.map(&{&1, y})
  end

  defp build_line({x1, y1}, {x2, y2}) do
    xi = if x1 <= x2, do: +1, else: -1
    yi = if y1 <= y2, do: +1, else: -1

    build_diag({x1, y1}, {x2, y2}, xi, yi)
  end

  defp build_diag(left, right, xi, yi, points \\ [])
  defp build_diag({x, y}, {x, y}, _xi, _yi, points), do: points ++ [{x, y}]

  defp build_diag({x1, y1}, right, xi, yi, points) do
    points = points ++ [{x1, y1}]
    next = {x1 + xi, y1 + yi}

    build_diag(next, right, xi, yi, points)
  end

  defp more_than_two(map) do
    map |> Map.values() |> Enum.filter(&(&1 >= 2)) |> Enum.count()
  end

  def star2(input) do
    input
    |> Enum.reduce(%{}, &build_map/2)
    |> more_than_two()
  end

  defp parse_line(line) do
    [left, right] = String.split(line, " -> ", trim: true)

    [lx, ly] = String.split(left, ",", trim: true) |> Enum.map(&String.to_integer/1)
    [rx, ry] = String.split(right, ",", trim: true) |> Enum.map(&String.to_integer/1)

    {{lx, ly}, {rx, ry}}
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
  end
end
