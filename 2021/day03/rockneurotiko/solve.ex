defmodule Aoc.Aoc2021.Day03.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    [first | rest] = input

    first = Enum.map(first, &%{&1 => 1})

    {gamma, epsilon} =
      Enum.reduce(rest, first, fn current, sums ->
        Enum.zip(current, sums)
        |> Enum.map(fn {n, s} ->
          update_in(s, [Access.key(n, 0)], &(&1 + 1))
        end)
      end)
      |> Enum.reduce({[], []}, fn elem, {max, min} ->
        {maxe, mine} = if elem["0"] > elem["1"], do: {"0", "1"}, else: {"1", "0"}

        {max ++ [maxe], min ++ [mine]}
      end)

    gamma = to_int(gamma)
    epsilon = to_int(epsilon)

    gamma * epsilon
  end

  defp to_int(number), do: number |> Enum.join("") |> Integer.parse(2) |> elem(0)

  def star2(input) do
    o2 =
      find_until_one(input, fn zeroes, ones ->
        lz = length(zeroes)
        lo = length(ones)

        cond do
          lz == lo -> ones
          lz > lo -> zeroes
          true -> ones
        end
      end)

    co2 =
      find_until_one(input, fn zeroes, ones ->
        lz = length(zeroes)
        lo = length(ones)

        cond do
          lz == lo -> zeroes
          lz > lo -> ones
          true -> zeroes
        end
      end)

    to_int(o2) * to_int(co2)
  end

  defp find_until_one(input, check) do
    c = Enum.count(input)

    Enum.reduce_while(0..(c - 1), input, fn
      _index, [solution] ->
        {:halt, solution}

      index, numbers ->
        {zeroes, ones} =
          Enum.reduce(numbers, {[], []}, fn elem, {zacc, oacc} ->
            if Enum.at(elem, index) == "0" do
              {zacc ++ [elem], oacc}
            else
              {zacc, oacc ++ [elem]}
            end
          end)

        {:cont, check.(zeroes, ones)}
    end)
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
