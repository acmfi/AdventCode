defmodule Aoc.Aoc2020.Day12.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    {_, {h, v}} = Enum.reduce(input, {"E", {0, 0}}, &move/2)

    abs(h) + abs(v)
  end

  defp move({"N", n}, {d, {h, v}}) do
    {d, {h, v + n}}
  end

  defp move({"S", n}, {d, {h, v}}) do
    {d, {h, v - n}}
  end

  defp move({"E", n}, {d, {h, v}}) do
    {d, {h + n, v}}
  end

  defp move({"W", n}, {d, {h, v}}) do
    {d, {h - n, v}}
  end

  defp move({"F", n}, {d, pos}) do
    move({d, n}, {d, pos})
  end

  defp move({a, n}, {d, {h, v}}) do
    {next_d(a, n, d), {h, v}}
  end

  defp next_d(_, 0, d), do: d

  defp next_d(a, n, d), do: next_d(a, n - 90, rot(a, d))

  defp rot("L", "E"), do: "N"
  defp rot("L", "N"), do: "W"
  defp rot("L", "W"), do: "S"
  defp rot("L", "S"), do: "E"
  defp rot("R", "E"), do: "S"
  defp rot("R", "N"), do: "E"
  defp rot("R", "W"), do: "N"
  defp rot("R", "S"), do: "W"

  # STAR 2

  def star2(input) do
    {_, {h, v}} = Enum.reduce(input, {{10, 1}, {0, 0}}, &move2/2)

    abs(h) + abs(v)
  end

  defp move2({a, n}, {{h, v}, pos}) when a in ["N", "S", "E", "W"] do
    {_, {h, v}} = move({a, n}, {"E", {h, v}})

    {{h, v}, pos}
  end

  defp move2({"F", n}, {{wh, wv}, {h, v}}) do
    nh = n * wh
    nv = n * wv

    {{wh, wv}, {h + nh, v + nv}}
  end

  defp move2({a, n}, {waypos, pos}) do
    {move_w(a, n, waypos), pos}
  end

  defp move_w(_a, 0, waypos), do: waypos

  defp move_w(a, deg, waypos) do
    move_w(a, deg - 90, move_w(a, waypos))
  end

  defp move_w("L", {h, v}), do: {v * -1, h}
  defp move_w("R", {h, v}), do: {v, h * -1}

  defp parse_line(<<d::binary-size(1)>> <> rest) do
    {d, String.to_integer(rest)}
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
