defmodule Aoc.Aoc2020.Day01.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    find_2020(input)
  end

  defp find_2020(list, extra \\ 0)
  defp find_2020([], _), do: 0

  defp find_2020([x | rest], extra) do
    result =
      Enum.reduce_while(rest, :not, fn y, _ ->
        if x + y + extra == 2020 do
          {:halt, {:ok, x, y, extra}}
        else
          {:cont, :not}
        end
      end)

    case result do
      {:ok, x, y, 0} -> x * y
      {:ok, x, y, z} -> x * y * z
      _ -> find_2020(rest, extra)
    end
  end

  def star2(input) do
    find3_2020(input)
  end

  defp find3_2020([]), do: 0

  defp find3_2020([x | rest]) do
    case find_2020(rest, x) do
      0 -> find3_2020(rest)
      result -> result
    end
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
