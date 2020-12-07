defmodule Aoc.Aoc2020.Day07.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    how_at_least(input, [{"shiny", "gold"}])
  end

  def star2(input) do
    deep_count_bags(input, {"shiny", "gold"})
  end

  defp how_at_least(bags, current, result \\ MapSet.new())

  defp how_at_least(_bags, [], result), do: MapSet.size(result)

  defp how_at_least(bags, [current | rest], result) do
    colors =
      bags
      |> Stream.filter(fn {color, bag} ->
        not MapSet.member?(result, color) and Map.has_key?(bag, current)
      end)
      |> Enum.map(fn {color, _} -> color end)

    result = Enum.reduce(colors, result, fn color, result -> MapSet.put(result, color) end)

    how_at_least(bags, rest ++ colors, result)
  end

  defp deep_count_bags(bags, color) do
    bags
    |> Map.get(color, %{})
    |> Enum.reduce(0, fn {color, n}, t ->
      t + n + n * deep_count_bags(bags, color)
    end)
  end

  defp parse_line(line) do
    [left, right] = String.split(line, " contain ")
    [var, color | _] = String.split(left, " ")
    left_bag = {var, color}

    right_bag =
      case right do
        "no other bags." ->
          %{}

        _ ->
          String.split(right, ",")
          |> Map.new(fn l ->
            [n, var, color, _] = l |> String.trim(".") |> String.trim(" ") |> String.split(" ")

            {{var, color}, String.to_integer(n)}
          end)
      end

    {left_bag, right_bag}
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
    |> Map.new(&parse_line/1)
  end
end
