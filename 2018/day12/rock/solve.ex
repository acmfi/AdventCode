defmodule Solve do
  @iterations 20

  def ex(path \\ "input_test") do
    input =
      path
      |> File.read!()
      |> String.split("\n", strip: true)
      |> parse_input()

    sol_1 = input |> grow_life(@iterations) |> Enum.sum()

    IO.puts("EX1: #{sol_1}")

    n = 2000
    {last_pot, diff} = stabilize(input, 2000)

    result = (50_000_000_000 - n) * diff + last_pot

    IO.puts("EX2: Variance of #{diff}, result: #{result}")
  end

  defp grow_life({pots, _rules}, 0), do: pots

  defp grow_life({pots, rules}, iterations) do
    new_pots = next_gen(pots, rules)

    grow_life({new_pots, rules}, iterations - 1)
  end

  defp next_gen(pots, rules) do
    left = Enum.min(pots) - 3
    right = Enum.max(pots) + 3

    Enum.reduce(Range.new(left, right), MapSet.new(), fn i, new_pots ->
      pot_check = Enum.map(-2..2, &pot_or_not(i + &1, pots))

      case MapSet.member?(rules, pot_check) do
        true -> MapSet.put(new_pots, i)
        _ -> new_pots
      end
    end)
  end

  defp stabilize(input, iteration, diffs \\ [])

  defp stabilize({pots, _}, 0, diffs) do
    diffs = Enum.take(diffs, 100)
    stab = div(Enum.sum(diffs), Enum.count(diffs))
    {Enum.sum(pots), stab}
  end

  defp stabilize({pots, rules}, iteration, diffs) do
    next_pots = next_gen(pots, rules)

    last_s = Enum.sum(pots)
    s = Enum.sum(next_pots)
    diff = s - last_s

    stabilize({next_pots, rules}, iteration - 1, [diff | diffs])
  end

  defp pot_or_not(pot, pots), do: if(MapSet.member?(pots, pot), do: "#", else: ".")

  defp parse_input([initial, "" | rules]) do
    rules = rules |> Enum.flat_map(&parse_rule/1) |> MapSet.new()
    "initial state: " <> initial_pots = initial

    pots = initial_pots |> String.codepoints() |> sparse()

    {pots, rules}
  end

  defp sparse(pots) do
    {set, _} =
      Enum.reduce(pots, {MapSet.new(), 0}, fn elem, {set, i} ->
        set =
          case elem do
            "#" -> MapSet.put(set, i)
            _ -> set
          end

        {set, i + 1}
      end)

    set
  end

  defp parse_rule(rule) do
    [pre, post] = String.split(rule, " => ", strip: true)

    case post do
      "#" -> [String.codepoints(pre)]
      _ -> []
    end
  end
end
