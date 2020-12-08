defmodule Aoc.Aoc2020.Day08.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    {:loop, acc} = loop(input)
    acc
  end

  def star2(input) do
    fix(input)
  end

  defp fix(input, prev \\ [])

  defp fix([{{i, x}, n} = instr | rest], prev) when i in ["nop", "jmp"] do
    left = prev ++ [{{fixed_ins(i), x}, n} | rest]

    case loop(left) do
      {:end, acc} ->
        acc

      _ ->
        fix(rest, prev ++ [instr])
    end
  end

  defp fix([instr | rest], prev), do: fix(rest, prev ++ [instr])

  defp fixed_ins("nop"), do: "jmp"
  defp fixed_ins("jmp"), do: "nop"

  defp loop(left, right \\ [], acc \\ 0)
  defp loop([], _right, acc), do: {:end, acc}
  defp loop([{_instr, 1} | _], _right, acc), do: {:loop, acc}

  defp loop([x | left], right, acc) do
    {left, right, acc} = exec(x, left, right, acc)
    loop(left, right, acc)
  end

  defp exec({{"nop", _} = instr, n}, left, right, acc), do: {left, [{instr, n + 1} | right], acc}

  defp exec({{"acc", an} = instr, n}, left, right, acc),
    do: {left, [{instr, n + 1} | right], acc + an}

  defp exec({{"jmp", j} = instr, n}, left, right, acc) do
    {left, right} = shift([{instr, n + 1} | left], right, j)
    {left, right, acc}
  end

  defp shift(left, right, 0), do: {left, right}

  defp shift(left, [], n) when n < 0, do: shift([], Enum.reverse(left), n)

  defp shift(left, [instr | right], n) when n < 0 do
    shift([instr | left], right, n + 1)
  end

  defp shift([], right, n) when n > 0, do: shift(Enum.reverse(right), [], n)

  defp shift([instr | left], right, n) when n > 0 do
    shift(left, [instr | right], n - 1)
  end

  defp parse_line(line) do
    [act, n] = String.split(line, " ")
    {{act, String.to_integer(n)}, 0}
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
