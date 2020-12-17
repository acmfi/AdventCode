defmodule Aoc.Aoc2020.Day14.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    init(input)
    |> Enum.map(fn {_, v} -> b_to_int(v) end)
    |> Enum.sum()
  end

  defp init(input, mem \\ %{}, mask \\ %{})

  defp init([], mem, _mask), do: mem

  defp init([{:mask, mask} | rest], mem, _mask) do
    init(rest, mem, parse_mask(mask))
  end

  defp init([{:mem, mn, v} | rest], mem, mask) do
    value = v |> parse_v() |> Map.merge(mask)

    mem = Map.put(mem, mn, value)

    init(rest, mem, mask)
  end

  defp b_to_int(m) do
    35..0
    |> Enum.map(&to_string(Map.get(m, &1, 0)))
    |> Enum.join("")
    |> Integer.parse(2)
    |> elem(0)
  end

  defp parse_mask(mask) do
    mask
    |> String.codepoints()
    |> Enum.with_index()
    |> Enum.filter(fn {x, _i} -> x != "X" end)
    |> Map.new(fn {x, i} -> {abs(i - 35), x} end)
  end

  def parse_v(v, f \\ ["1"]) do
    l = v |> Integer.to_string(2) |> String.codepoints()
    n = length(l) - 1

    l
    |> Enum.with_index()
    |> Enum.filter(fn {x, _i} -> x in f end)
    |> Map.new(fn {x, i} -> {abs(n - i), x} end)
  end

  def star2(input) do
    init2(input) |> Map.values() |> Enum.sum()
  end

  defp init2(input, mem \\ %{}, mask \\ %{})

  defp init2([], mem, _m), do: mem

  defp init2([{:mask, m} | rest], mem, _m) do
    init2(rest, mem, parse_mask2(m))
  end

  defp init2([{:mem, mn, v} | rest], mem, mask) do
    mem =
      parse_v(String.to_integer(mn), ["0", "1"])
      |> apply_mask(mask)
      |> expand_mask()
      |> Enum.reduce(mem, fn d, mem ->
        Map.put(mem, b_to_int(d), v)
      end)

    init2(rest, mem, mask)
  end

  defp apply_mask(n, mask) do
    Enum.reduce(mask, n, fn
      {i, "1"}, n -> Map.put(n, i, "1")
      {i, "X"}, n -> Map.put(n, i, "X")
      _, n -> n
    end)
  end

  defp expand_mask(n) do
    Enum.reduce(n, [%{}], fn
      {i, "X"}, acc ->
        Enum.flat_map(acc, fn m -> [Map.put(m, i, "0"), Map.put(m, i, "1")] end)

      {i, v}, acc ->
        Enum.map(acc, &Map.put(&1, i, v))
    end)
  end

  defp parse_mask2(mask) do
    mask
    |> String.codepoints()
    |> Enum.with_index()
    |> Map.new(fn {x, i} -> {abs(i - 35), x} end)
  end

  defp parse_line(line) do
    [c, v] = String.split(line, " = ")

    case c do
      "mask" ->
        {:mask, v}

      "mem[" <> r ->
        m = String.trim(r, "]")
        {:mem, m, String.to_integer(v)}
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
    |> Enum.map(&parse_line/1)
  end
end
