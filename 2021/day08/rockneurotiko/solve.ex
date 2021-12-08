defmodule Aoc.Aoc2021.Day08.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    input
    |> Enum.flat_map(fn {_l, right} -> right end)
    |> Enum.filter(&simple_digit?/1)
    |> Enum.count()
  end

  @simple_lengths [2, 3, 4, 7]
  defp simple_digit?(l) when length(l) in @simple_lengths, do: true
  defp simple_digit?(_), do: false

  def star2(input) do
    input |> Enum.map(&decipher/1) |> Enum.sum()
  end

  defp decipher({patterns, output}) do
    sorted = patterns |> Enum.sort_by(&length/1) |> Enum.map(&Enum.sort/1)

    [one, seven, four | _] = sorted

    Enum.map(output, &determine_number(&1, one, seven, four))
    |> Enum.join("")
    |> String.to_integer()
  end

  defp determine_number(number, one, seven, four) do
    number = Enum.sort(number)

    case number do
      ^one -> "1"
      ^four -> "4"
      ^seven -> "7"
      _ -> determine_by_substracts(number, one, four)
    end
  end

  defp determine_by_substracts(number, one, four) do
    case length(number) do
      5 -> five_segments(number, one, four)
      6 -> six_segments(number, one, four)
      7 -> "8"
    end
  end

  defp five_segments(number, one, four) do
    case length(number -- one) do
      3 ->
        "3"

      _ ->
        case length(number -- four) do
          3 -> "2"
          _ -> "5"
        end
    end
  end

  defp six_segments(number, one, four) do
    case length(number -- one) do
      5 ->
        "6"

      _ ->
        case length(number -- four) do
          2 -> "9"
          _ -> "0"
        end
    end
  end

  defp parse_line(line) do
    [left, right] = String.split(line, " | ", trim: true)

    {parse_letters(left), parse_letters(right)}
  end

  defp parse_letters(input),
    do: input |> String.split(" ", trim: true) |> Enum.map(&String.graphemes/1)

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
