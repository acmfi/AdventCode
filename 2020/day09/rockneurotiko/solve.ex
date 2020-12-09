defmodule Aoc.Aoc2020.Day09.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input, n) do
    find_first(input, n)
  end

  def star2(input, n) do
    i = find_first(input, n)
    {min, max} = find_weak(input, i)
    min + max
  end

  defp find_weak(input, n, group \\ [], acc \\ 0)

  defp find_weak([x | rest], n, _group, _acc) when x >= n, do: find_weak(rest, n, [], 0)

  defp find_weak([x | _rest], n, group, acc) when x + acc == n and length(group) >= 2 do
    g = [x | group]
    {Enum.min(g), Enum.max(g)}
  end

  defp find_weak([x | rest], n, group, acc) when x + acc < n do
    group = group ++ [x]
    acc = x + acc
    find_weak(rest, n, group, acc)
  end

  defp find_weak([x | _] = input, n, group, acc) when x + acc > n do
    {dropped, group} = Enum.split(group, 1)
    d = Enum.at(dropped, 0) || 0
    acc = acc - d
    find_weak(input, n, group, acc)
  end

  defp find_first(input, n) do
    {pream, rest} = Enum.split(input, n)

    loop(pream, rest)
  end

  defp loop(_pream, []), do: :ok

  defp loop(pream, [x | rest]) do
    case valid?(x, pream) do
      true ->
        pream = Enum.drop(pream, 1)

        loop(pream ++ [x], rest)

      _ ->
        x
    end
  end

  defp valid?(_x, []), do: false

  defp valid?(x, [n | rest]) do
    found =
      Enum.reduce_while(rest, false, fn
        ^n, _ ->
          {:cont, false}

        n2, _ ->
          case x == n + n2 do
            true -> {:halt, true}
            _ -> {:cont, false}
          end
      end)

    case found do
      true -> true
      _ -> valid?(x, rest)
    end
  end

  defp parse_line(line) do
    String.to_integer(line)
  end

  # Helpers

  def solve(fname \\ "input") do
    do_solve(fname, 25)
  end

  def solve_test(), do: do_solve("input_test", 5)

  defp do_solve(fname, n) do
    path = Path.join(@base_path, fname)

    input = path |> File.read!() |> parse!()
    star1 = star1(input, n)
    IO.puts("Star 1: " <> to_string(star1))

    star2 = star2(input, n)
    IO.puts("Star 2: " <> to_string(star2))
  end

  defp parse!(t) do
    t
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
  end
end
