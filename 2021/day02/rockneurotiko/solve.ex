defmodule Aoc.Aoc2021.Day02.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    {forward, depth} = Enum.reduce(input, {0, 0}, &reduce_action_1/2)

    forward * depth
  end

  defp reduce_action_1({"forward", step}, {forward, depth}), do: {forward + step, depth}
  defp reduce_action_1({"down", step}, {forward, depth}), do: {forward, depth + step}
  defp reduce_action_1({"up", step}, {forward, depth}), do: {forward, depth - step}

  def star2(input) do
    {forward, depth, _aim} = Enum.reduce(input, {0, 0, 0}, &reduce_action_2/2)

    forward * depth
  end

  defp reduce_action_2({"forward", step}, {forward, depth, aim}),
    do: {forward + step, depth + aim * step, aim}

  defp reduce_action_2({"down", step}, {forward, depth, aim}), do: {forward, depth, aim + step}
  defp reduce_action_2({"up", step}, {forward, depth, aim}), do: {forward, depth, aim - step}

  defp parse_line(line) do
    case String.split(line, " ") do
      [action, step] -> {action, String.to_integer(step)}
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
    t
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
  end
end
