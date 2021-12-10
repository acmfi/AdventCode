defmodule Aoc.Aoc2021.Day10.Solve do
  @base_path Path.dirname(__ENV__.file)

  @open ["{", "(", "[", "<"]
  @close ["}", ")", "]", ">"]
  @close_for Enum.zip(@open, @close) |> Map.new()
  @elem_points %{")" => 3, "]" => 57, "}" => 1197, ">" => 25137}

  def star1(input) do
    input
    |> Enum.map(&find_first_corrupted/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.map(&elem_to_points/1)
    |> Enum.sum()
  end

  defp elem_to_points(elem) when elem in @close, do: @elem_points[elem]

  def star2(input) do
    points =
      input
      |> Enum.map(&full_line/1)
      |> Enum.filter(&is_invalid?/1)
      |> Enum.map(&close_points/1)
      |> Enum.sort()

    middle = div(Enum.count(points), 2)

    Enum.at(points, middle)
  end

  @close_points %{")" => 1, "]" => 2, "}" => 3, ">" => 4}

  defp close_points({:invalid, left_open}) do
    Enum.reduce(left_open, 0, fn open, acc ->
      acc * 5 + @close_points[@close_for[open]]
    end)
  end

  defp is_invalid?({:invalid, _}), do: true
  defp is_invalid?(_), do: false

  defp full_line(line) do
    case find_chunk(line) do
      {:ok, _chunk, []} -> :ok
      {:ok, _chunk, rest} -> full_line(rest)
      other -> other
    end
  end

  defp find_first_corrupted(line) do
    case find_chunk(line) do
      {:corrupted, elem} -> elem
      _ -> nil
    end
  end

  defp find_chunk(line, curr \\ nil, acc \\ [])

  defp find_chunk([], _elem, acc), do: {:invalid, acc}

  defp find_chunk([elem | line], nil, _acc) when elem in @open do
    find_chunk(line, elem, [elem])
  end

  defp find_chunk([elem | _line], nil, _acc), do: {:corrupted, elem}

  defp find_chunk([elem | line], open, acc) when elem in @close do
    if @close_for[open] == elem do
      acc = [elem | acc]
      chunk = Enum.reverse(acc)

      {:ok, chunk, line}
    else
      {:corrupted, elem}
    end
  end

  defp find_chunk([elem | line], open, acc) when elem in @open do
    case find_chunk(line, elem, [elem]) do
      {:ok, chunk, []} ->
        {:ok, chunk, acc}

      {:ok, _chunk, line} ->
        find_chunk(line, open, acc)

      {:invalid, chunk_acc} ->
        {:invalid, chunk_acc ++ acc}

      error ->
        error
    end
  end

  defp parse_line(line) do
    String.graphemes(line)
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
