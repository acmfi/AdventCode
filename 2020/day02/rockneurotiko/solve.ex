defmodule Aoc.Aoc2020.Day02.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    input |> Enum.map(&star1_is_pwd_valid?/1) |> Enum.filter(& &1) |> Enum.count()
  end

  defp star1_is_pwd_valid?({rules, pwd}) do
    counted = count_letters(pwd)

    Enum.all?(rules, fn {letter, {min, max}} ->
      lc = Map.get(counted, letter, 0)
      lc >= min and lc <= max
    end)
  end

  defp count_letters(pwd, counted \\ %{})

  defp count_letters("", counted), do: counted

  defp count_letters(<<letter::binary-size(1)>> <> pwd, counted) do
    count_letters(pwd, update_in(counted, [Access.key(letter, 0)], &(&1 + 1)))
  end

  def star2(input) do
    input |> Enum.map(&star2_is_pwd_valid?/1) |> Enum.filter(& &1) |> Enum.count()
  end

  defp star2_is_pwd_valid?({rules, pwd}) do
    Enum.all?(rules, fn {letter, {min, max}} ->
      one_position?(pwd, letter, [min, max])
    end)
  end

  defp one_position?(pwd, letter, positions) do
    {_how_many, valid} =
      pwd
      |> String.codepoints()
      |> Enum.with_index()
      |> Enum.reduce_while({0, false}, fn {pwd_letter, index}, {acc, v} ->
        matched = pwd_letter == letter and (index + 1) in positions

        cond do
          matched and acc == 0 -> {:cont, {acc + 1, true}}
          matched and acc >= 1 -> {:halt, {acc + 1, false}}
          true -> {:cont, {acc, v}}
        end
      end)

    valid
  end

  defp parse_line(line) do
    [rule, letter, pwd] = String.split(line, " ")
    [min, max] = String.split(rule, "-") |> Enum.map(&String.to_integer/1)
    letter = String.trim(letter, ":")

    {%{letter => {min, max}}, pwd}
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
    |> String.split("\n")
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&parse_line/1)
  end
end
