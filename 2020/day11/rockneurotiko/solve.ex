defmodule Aoc.Aoc2020.Day11.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1({input, n}) do
    stabilize(input, :first) |> print_map(n) |> count_occupied()
  end

  def star2({input, n}) do
    stabilize(input, :second) |> print_map(n) |> count_occupied()
  end

  defp count_occupied(map) do
    Enum.filter(map, fn {_, v} -> v == "#" end) |> Enum.count()
  end

  defp stabilize(map, star, prev \\ nil)
  defp stabilize(map, _star, map2) when map == map2, do: map

  defp stabilize(map, star, _prev) do
    map |> step(star) |> stabilize(star, map)
  end

  defp step(map, star) do
    Enum.reduce(map, %{}, fn {{i, j}, v}, new_map ->
      s = new_state(v, i, j, map, star)
      Map.put(new_map, {i, j}, s)
    end)
  end

  defp new_state(".", _i, _j, _map, _star), do: "."

  defp new_state("L", i, j, map, star) do
    free_positions(star, map, i, j)
    |> Enum.all?()
    |> case do
      true -> "#"
      _ -> "L"
    end
  end

  defp new_state("#", i, j, map, star) do
    count =
      free_positions(star, map, i, j)
      |> Enum.filter(&(not &1))
      |> Enum.count()

    if count >= occupied_to_free(star), do: "L", else: "#"
  end

  defp occupied_to_free(:first), do: 4
  defp occupied_to_free(_), do: 5

  defp free_positions(star, map, i, j) do
    find_seats(star, map, i, j)
    |> Enum.map(&(&1 == "L"))
  end

  defp find_seats(:first, map, i, j) do
    for is <- Range.new(i - 1, i + 1), js <- Range.new(j - 1, j + 1), {is, js} != {i, j} do
      Map.get(map, {is, js}, ".")
    end
    |> Enum.filter(&(&1 != "."))
  end

  @searchs for i <- -1..1, j <- -1..1, {i, j} != {0, 0}, do: {i, j}
  defp find_seats(_, map, i, j) do
    Enum.map(@searchs, &search_seat(map, i, j, &1))
    |> Enum.filter(&(&1 != "."))
  end

  defp search_seat(map, i, j, {ii, ij}) do
    do_search_seat(map, i + ii, j + ij, {ii, ij})
  end

  defp do_search_seat(map, i, j, {ii, ij}) do
    case Map.get(map, {i, j}) do
      nil -> "."
      "." -> do_search_seat(map, i + ii, j + ij, {ii, ij})
      other -> other
    end
  end

  defp print_map(map, n) do
    Enum.map(Range.new(0, n), fn i ->
      Enum.map(Range.new(0, n), &Map.get(map, {i, &1})) |> Enum.join("")
    end)
    |> Enum.join("\n")
    |> IO.puts()

    IO.puts("-----------")
    map
  end

  defp parse_line(line) do
    String.codepoints(line)
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
    |> to_map_matrix()
  end

  defp to_map_matrix(input) do
    n = Enum.count(input)

    map =
      input
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {row, i}, map ->
        Enum.with_index(row) |> Enum.reduce(map, fn {v, j}, map -> Map.put(map, {i, j}, v) end)
      end)

    {map, n}
  end
end
