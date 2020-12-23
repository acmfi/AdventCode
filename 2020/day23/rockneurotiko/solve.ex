defmodule Aoc.Aoc2020.Day23.Solve do
  @base_path Path.dirname(__ENV__.file)

  defmodule Zip do
    def new(l) do
      {l, []}
    end

    def next({[], r}), do: next({Enum.reverse(r), []})

    def next({[x | l], r}), do: {l, [x | r]}

    def pop({[], r}), do: pop({Enum.reverse(r), []})
    def pop({[x | l], r}), do: {x, {l, r}}

    def insert_many_at({[], r}, n, vs) do
      insert_many_at({Enum.reverse(r), []}, n, vs)
    end

    def insert_many_at({[n | l], r}, n, vs), do: {[n | vs] ++ l, r}

    def insert_many_at({[nn | l], r}, n, vs) do
      {nl, nr} = insert_many_at({l, r}, n, vs)
      {[nn | nl], nr}
    end

    def value({[], r}), do: value({Enum.reverse(r), []})

    def value({[x | _], _r} = zip), do: {x, zip}

    def to_list({l, r}), do: l ++ Enum.reverse(r)
  end

  alias __MODULE__.Zip

  def star1(input) do
    {l, [1 | r]} = play(input, 100, 9) |> Enum.split_while(&(&1 != 1))

    (r ++ l) |> Enum.join("")
  end

  def star2(input) do
    input = input ++ (Range.new(length(input) + 1, 1_000_000) |> Enum.to_list())

    {_, [1, n1, n2 | _]} = play(input, 10_000_000, 1_000_000) |> Enum.split_while(&(&1 != 1))

    n1 * n2
  end

  defp play(cups, n, max) when is_list(cups), do: play(Zip.new(cups), n, max)

  defp play(cups, 0, _max), do: Zip.to_list(cups)

  defp play(cups, n, max) do
    if Integer.mod(n, 100) == 0, do: IO.puts(n)

    cups |> play_round(max) |> play(n - 1, max)
  end

  defp play_round(cups, max) do
    {current, cups} = Zip.value(cups)

    cups = Zip.next(cups)

    {cups, ps} =
      0..2
      |> Enum.reduce({cups, []}, fn _, {cups, acc} ->
        {n, cups} = Zip.pop(cups)

        {cups, acc ++ [n]}
      end)

    next_n = select_next(current - 1, ps, max)

    Zip.insert_many_at(cups, next_n, ps)
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
