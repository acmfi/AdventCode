defmodule Aoc.Aoc2020.Day04.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    input |> Enum.filter(&all_fields?/1) |> Enum.count()
  end

  @keys [
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
    # "cid",
  ]
  defp all_fields?(input) do
    Enum.all?(@keys, fn key ->
      Map.has_key?(input, key)
    end)
  end

  def star2(input) do
    input
    |> Enum.filter(&all_fields?/1)
    |> Enum.filter(&valid_passport?/1)
    |> Enum.count()
  end

  defp valid_passport?(psp) do
    Enum.all?(psp, &valid_field?/1)
  end

  defp valid_field?(f) do
    try do
      valid_field(f)
    rescue
      _ -> false
    end
  end

  defp valid_field({"byr", str}) do
    i = parse_integer(str)
    i >= 1920 and i <= 2002
  end

  defp valid_field({"iyr", str}) do
    i = parse_integer(str)
    i >= 2010 and i <= 2020
  end

  defp valid_field({"eyr", str}) do
    i = parse_integer(str)
    i >= 2020 and i <= 2030
  end

  defp valid_field({"hgt", str}) do
    {hg, ms} = Integer.parse(str)

    case ms do
      "cm" -> hg >= 150 and hg <= 193
      "in" -> hg >= 59 and hg <= 76
    end
  end

  @valid_color_chars (0..9 |> Enum.map(&to_string/1)) ++ ["a", "b", "c", "d", "e", "f"]
  defp valid_field({"hcl", "#" <> color}) do
    cd = String.codepoints(color)
    Enum.count(cd) == 6 and Enum.all?(cd, &(&1 in @valid_color_chars))
  end

  defp valid_field({"hcl", _}), do: false

  defp valid_field({"ecl", str}) do
    str in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  end

  defp valid_field({"pid", str}) do
    i = parse_integer(str)
    String.length(str) == 9 and i <= 999_999_999
  end

  defp valid_field({"cid", _}), do: true
  defp valid_field(_), do: false

  defp parse_line(""), do: []

  defp parse_line(line) do
    String.split(line, " ") |> Enum.map(&(String.split(&1, ":") |> List.to_tuple()))
  end

  defp parse_integer(x) do
    {int, ""} = Integer.parse(x)
    int
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
    |> Enum.map(&parse_line/1)
    |> reparse()
    |> Enum.map(&Map.new/1)
  end

  defp reparse(input) do
    {acc, row} =
      Enum.reduce(input, {[], []}, fn
        [], {acc, row} -> {acc ++ [row], []}
        row, {acc, acc_row} -> {acc, acc_row ++ row}
      end)

    case row do
      [] -> acc
      _ -> acc ++ [row]
    end
  end
end
