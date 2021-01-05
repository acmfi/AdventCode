defmodule Aoc.Aoc2020.Day21.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    {foods, poss} = analyze(input)

    all_foods = MapSet.new(Map.keys(foods))

    bad = Enum.reduce(Map.values(poss), MapSet.new(), &MapSet.union/2)

    MapSet.difference(all_foods, bad)
    |> Enum.map(&Map.get(foods, &1))
    |> Enum.sum()
  end

  def star2(input) do
    {_foods, poss} = analyze(input)

    poss |> simplify() |> Enum.sort_by(&elem(&1, 0)) |> Enum.map(&elem(&1, 1)) |> Enum.join(",")
  end

  defp simplify(poss, acc \\ %{}, seen \\ MapSet.new())

  defp simplify(poss, acc, _seen) when map_size(poss) == 0, do: acc

  defp simplify(poss, acc, seen) do
    {name, alls} =
      Enum.find(poss, fn {_k, v} -> v |> MapSet.difference(seen) |> MapSet.size() == 1 end)

    allerg = MapSet.difference(alls, seen) |> Enum.at(0)

    poss = Map.delete(poss, name)
    acc = Map.put(acc, name, allerg)
    seen = MapSet.put(seen, allerg)

    simplify(poss, acc, seen)
  end

  defp analyze(input) do
    Enum.reduce(input, {%{}, %{}}, fn {food, allgs}, {foods, poss} ->
      foods = Enum.reduce(food, foods, fn x, y -> Map.update(y, x, 1, &(&1 + 1)) end)

      poss =
        Enum.reduce(allgs, poss, fn allg, poss ->
          Map.update(poss, allg, food, &MapSet.intersection(&1, food))
        end)

      {foods, poss}
    end)
  end

  defp parse_line(line) do
    [foodr, allr] = String.split(line, " (contains ", trim: true)

    food = String.split(foodr, " ", trim: true) |> MapSet.new()

    allerg = allr |> String.trim(")") |> String.split(", ") |> MapSet.new()

    {food, allerg}
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
