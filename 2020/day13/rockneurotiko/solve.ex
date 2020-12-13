defmodule Aoc.Aoc2020.Day13.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1({t, bs}) do
    bs = Enum.filter(bs, &is_integer/1)
    {t2, b} = find_nearest(t, bs)

    (t2 - t) * b
  end

  defp find_nearest(t, bs) do
    case Enum.find(bs, fn b -> :math.fmod(t, b) == 0.0 end) do
      nil -> find_nearest(t + 1, bs)
      b -> {t, b}
    end
  end

  def star2({_, bs}) do
    bs = bs |> Enum.with_index() |> Enum.filter(fn {x, _} -> is_integer(x) end)

    find_seq(100_000_000_000_000, bs)
  end

  defp find_seq(t, bs) do
    Enum.reduce(bs, {t, 1}, fn {b, i}, {t, step} ->
      nt =
        Stream.iterate(t, &(&1 + step))
        |> Stream.filter(fn t ->
          :math.fmod(t + i, b) == 0.0
        end)
        |> Enum.at(0)

      {nt, step * b}
    end)
    |> elem(0)
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
    [t, bs] = String.split(t, "\n", trim: true)

    {String.to_integer(t),
     bs
     |> String.split(",", trim: true)
     |> Enum.map(fn
       "x" -> "x"
       o -> String.to_integer(o)
     end)}
  end
end
