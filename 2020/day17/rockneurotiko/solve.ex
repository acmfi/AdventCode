defmodule Aoc.Aoc2020.Day17.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    Enum.reduce(1..6, input, fn _, c -> cycle(c) end) |> MapSet.size()
  end

  def star2(input) do
    input = Enum.map(input, fn {x, y, z} -> {x, y, z, 0} end) |> MapSet.new()

    Enum.reduce(1..6, input, fn _, c -> cycle(c) end) |> MapSet.size()
  end

  defp cycle(c) do
    new_actives = new_actives(c)
    keep_active = keep_active(c)
    MapSet.new(keep_active ++ new_actives)
  end

  defp keep_active(c) do
    Enum.reduce(c, [], fn pos, acc ->
      count = pos |> neighs() |> Enum.filter(&MapSet.member?(c, &1)) |> Enum.count()

      if count == 2 or count == 3 do
        [pos | acc]
      else
        acc
      end
    end)
  end

  defp new_actives(c) do
    Enum.reduce(c, %{}, fn pos, acc ->
      Enum.reduce(neighs(pos), acc, fn p, acc ->
        Map.update(acc, p, 1, &(&1 + 1))
      end)
    end)
    |> Enum.filter(fn
      {_, 3} -> true
      _ -> false
    end)
    |> Enum.filter(fn {p, _} -> not MapSet.member?(c, p) end)
    |> Enum.map(&elem(&1, 0))
  end

  def neighs({x, y, z} = orig_pos) do
    for i <- -1..1, j <- -1..1, k <- -1..1, pos = {x + i, y + j, z + k}, pos != orig_pos do
      pos
    end
  end

  def neighs({x, y, z, w} = orig_pos) do
    for i <- -1..1,
        j <- -1..1,
        k <- -1..1,
        l <- -1..1,
        pos = {x + i, y + j, z + k, w + l},
        pos != orig_pos do
      pos
    end
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
    |> Enum.with_index()
    |> Enum.flat_map(fn {l, y} ->
      String.codepoints(l)
      |> Enum.with_index()
      |> Enum.filter(&(elem(&1, 0) == "#"))
      |> Enum.map(fn {_, x} -> {x, y, 0} end)
    end)
    |> MapSet.new()
  end
end
