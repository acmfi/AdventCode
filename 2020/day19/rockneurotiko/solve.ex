defmodule Aoc.Aoc2020.Day19.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1({rules, input}) do
    reg = comp(re("0", rules))

    Enum.filter(input, fn i ->
      rmatch?(reg, i)
    end)
    |> Enum.count()
  end

  def star2({rules, input}) do
    reg = comp(re("0", rules, true))

    Enum.filter(input, fn i ->
      rmatch?(reg, i)
    end)
    |> Enum.count()
  end

  # I have to use re2 instead of Regex or re because
  # the erlang standard regex library has a size limit
  # that we are breaking :)

  defp comp(r) do
    {:ok, reg} = :re2.compile("^#{r}$")
    reg
  end

  defp rmatch?(reg, i) do
    case :re2.run(i, reg) do
      :nomatch -> false
      _ -> true
    end
  end

  defp re(rn, rules, p2 \\ false)

  # 8: 42 | 42 8
  defp re("8", rules, true) do
    re("42", rules, true) <> "+"
  end

  # 11: 42 31 | 42 11 31
  defp re("11", rules, true) do
    r42 = re("42", rules, true)
    r31 = re("31", rules, true)

    rt =
      1..10
      |> Enum.map(fn i ->
        "#{r42}{#{i}}#{r31}{#{i}}"
      end)
      |> Enum.join("|")

    "(?:#{rt})"
  end

  defp re(rn, rules, p2) do
    case Map.get(rules, rn) do
      {:str, c} ->
        c

      {:ind, inds} ->
        re_inds(inds, rules, p2)
    end
  end

  defp re_inds(inds, rules, p2) do
    r =
      Enum.map(inds, fn ind ->
        Enum.map(ind, &re(&1, rules, p2)) |> Enum.join("")
      end)
      |> Enum.join("|")

    "(?:#{r})"
  end

  defp parse_rules(rules) do
    Enum.reduce(rules, %{}, fn raw, acc ->
      [id, rule] = String.split(raw, ":")
      rule = String.trim(rule, " ")

      Map.put(acc, id, parse_rule(rule))
    end)
  end

  defp parse_rule("\"" <> r), do: {:str, String.trim(r, "\"")}

  defp parse_rule(rule) do
    r = String.split(rule, " | ") |> Enum.map(&String.split(&1, " "))
    {:ind, r}
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
    [rules, text] =
      String.split(t, "\n\n", trim: true) |> Enum.map(&String.split(&1, "\n", trim: true))

    {parse_rules(rules), text}
  end
end
