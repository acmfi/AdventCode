defmodule Aoc.Aoc2020.Day23.Solve do
  @base_path Path.dirname(__ENV__.file)

  defmodule MDL do
    def new([]), do: {%{}, nil}

    def new([n | rest]) do
      {last, acc} =
        Enum.reduce(rest, {n, %{}}, fn e, {p, acc} ->
          {e, Map.put(acc, p, e)}
        end)

      {Map.put(acc, last, n), n}
    end

    def next({st, c}) do
      {st, Map.get(st, c)}
    end

    def value({_st, c}), do: c

    def insert({st, c}, n) do
      next = Map.get(st, c)

      {st |> Map.put(c, n) |> Map.put(n, next), c}
    end

    def find!({st, _c}, n), do: {st, n}

    def pop_next({mdl, c}) do
      pop = Map.get(mdl, c)

      mdl = mdl |> Map.put(c, Map.get(mdl, pop)) |> Map.delete(pop)

      {pop, {mdl, c}}
    end

    def to_list({mdl, _c}) when mdl == %{}, do: []

    def to_list({mdl, c}) do
      {n, mdl} = Map.pop(mdl, c)

      [c | to_list({mdl, n})]
    end
  end

  alias __MODULE__.MDL

  def star1(input) do
    {l, [1 | r]} = play(input, 100, 9) |> Enum.split_while(&(&1 != 1))

    (r ++ l) |> Enum.join("")
  end

  def star2(input) do
    input = input ++ (Range.new(length(input) + 1, 1_000_000) |> Enum.to_list())

    {_, [1, n1, n2 | _]} = play(input, 10_000_000, 1_000_000) |> Enum.split_while(&(&1 != 1))

    n1 * n2
  end

  defp play(cups, n, max) when is_list(cups), do: play(MDL.new(cups), n, max)

  defp play(cups, 0, _max), do: MDL.to_list(cups)

  defp play(cups, n, max) do
    cups |> play_round(max) |> play(n - 1, max)
  end

  defp play_round(cups, max) do
    current = MDL.value(cups)

    {cups, ps} =
      0..2
      |> Enum.reduce({cups, []}, fn _, {cups, acc} ->
        {n, cups} = MDL.pop_next(cups)

        {cups, acc ++ [n]}
      end)

    next_n = select_next(current - 1, ps, max)

    ps
    |> Enum.reduce(MDL.find!(cups, next_n), fn p, cups ->
      cups |> MDL.insert(p) |> MDL.next()
    end)
    |> MDL.find!(current)
    |> MDL.next()
  end

  defp select_next(c, picks, max) when c < 1, do: select_next(max, picks, max)

  defp select_next(c, picks, max) do
    if c in picks do
      select_next(c - 1, picks, max)
    else
      c
    end
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
    t |> String.trim() |> String.codepoints() |> Enum.map(&String.to_integer/1)
  end
end
