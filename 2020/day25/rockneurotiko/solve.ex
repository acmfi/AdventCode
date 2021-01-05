defmodule Aoc.Aoc2020.Day25.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1([card, door]) do
    card |> find_loop() |> secret(door)
  end

  defp secret(i, sub) do
    1..i
    |> Enum.reduce(1, fn _, curr ->
      loop(curr, sub)
    end)
  end

  defp find_loop(curr \\ 1, sub \\ 7, n, loops \\ 0)

  defp find_loop(curr, _sub, curr, loops), do: loops

  defp find_loop(curr, sub, n, loops) do
    find_loop(loop(curr, sub), sub, n, loops + 1)
  end

  defp loop(curr, sub) do
    Integer.mod(curr * sub, 20_201_227)
  end

  defp parse_line(line) do
    String.to_integer(line)
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

    IO.puts("Star 2: Just click on the web if you have 49 stars!")
  end

  defp parse!(t) do
    t
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
  end
end
