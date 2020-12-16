defmodule Aoc.Aoc2020.Day16.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1({rules, _mine, tickets}) do
    error_rate(tickets, rules)
  end

  defp error_rate(tickets, rules, error \\ 0)
  defp error_rate([], _rules, error), do: error

  defp error_rate([ticket | tickets], rules, error) do
    {_ok, errors} = validate(ticket, rules)

    error = Enum.sum(errors) + error

    error_rate(tickets, rules, error)
  end

  defp validate(ticket, rules) do
    Enum.reduce(ticket, {[], []}, fn n, {ok, err} ->
      case Enum.any?(rules, &validate_rule(&1, n)) do
        true -> {ok ++ [n], err}
        _ -> {ok, err ++ [n]}
      end
    end)
  end

  defp validate_rule({_name, ranges}, n) do
    Enum.any?(ranges, &in_range?(n, &1))
  end

  defp in_range?(n, [l, u]) when n >= l and n <= u, do: true
  defp in_range?(_n, _range), do: false

  def star2({rules, mine, tickets}) do
    [mine | tickets]
    |> discard_invalids(rules)
    |> find_positions(rules)
    |> simplify_positions()
    |> Enum.filter(fn {name, _} -> String.starts_with?(name, "departure") end)
    |> Enum.map(fn {_name, i} ->
      Enum.at(mine, i)
    end)
    |> Enum.reduce(1, &*/2)
  end

  defp simplify_positions(positions, detected \\ %{}) do
    case Enum.find(positions, fn {_name, p} -> MapSet.size(p) == 1 end) do
      nil ->
        detected

      {name, p} ->
        [i] = MapSet.to_list(p)
        detected = Map.put(detected, name, i)

        positions =
          Map.delete(positions, name) |> Map.new(fn {name, p} -> {name, MapSet.delete(p, i)} end)

        simplify_positions(positions, detected)
    end
  end

  defp discard_invalids(tickets, rules) do
    Enum.filter(tickets, fn t ->
      case validate(t, rules) do
        {_, []} -> true
        _ -> false
      end
    end)
  end

  defp find_positions(tickets, rules) do
    n_rules = Enum.count(rules)

    base_positions = Map.new(rules, fn {name, _} -> {name, MapSet.new(Range.new(0, n_rules))} end)

    Enum.reduce(tickets, base_positions, fn ticket, positions ->
      Enum.reduce(rules, positions, fn {name, _ranges} = rule, positions ->
        valids = MapSet.new(valid_positions(ticket, rule))

        update_in(positions, [name], &MapSet.intersection(&1, valids))
      end)
    end)
  end

  defp valid_positions(ticket, rule) do
    Enum.map(ticket, &validate_rule(rule, &1))
    |> Enum.with_index()
    |> Enum.filter(&elem(&1, 0))
    |> Enum.map(&elem(&1, 1))
  end

  defp parse_rules(rules) do
    Map.new(rules, &parse_rule/1)
  end

  defp parse_rule(rule) do
    [name, rule] = String.split(rule, ": ")

    rule =
      String.split(rule, " or ")
      |> Enum.map(fn r ->
        String.split(r, "-") |> Enum.map(&String.to_integer/1)
      end)

    {name, rule}
  end

  defp parse_ticket(ticket) do
    String.split(ticket, ",", trim: true) |> Enum.map(&String.to_integer/1)
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
    [rules, [_, mine], [_ | tickets]] =
      String.split(t, "\n\n", trim: true) |> Enum.map(&String.split(&1, "\n", trim: true))

    {parse_rules(rules), parse_ticket(mine), Enum.map(tickets, &parse_ticket/1)}
  end
end
