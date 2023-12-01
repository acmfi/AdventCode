defmodule Cuwano do
  def test(star) do
    parse_and_solve("test_input", star)
  end

  def solve(star) do
    parse_and_solve("input", star)
  end

  defp parse_and_solve(file_name, star) do
    solve = if star in [:star1, 1], do: &star1/1, else: &star2/1

    file_name |> parse_input() |> solve.()
  end

  defp parse_input(file) do
    file |> File.read!() |> parse()
  end

  def parse(text), do: String.split(text, "\n")

  def star1(input) do
    input |> Enum.map(&find_pairs/1) |> Enum.sum()
  end

  defp find_pairs(string) do
    first = find_first_number(string)
    last = string |> String.reverse() |> find_first_number()

    String.to_integer(first <> last)
  end

  defp find_first_number(string) do
    ~r/([a-zA-Z]*)(?<number>\d)/
    |> Regex.named_captures(string)
    |> Map.fetch!("number")
  end

  def star2(input) do
    input |> Enum.map(&find_spelled_pairs/1) |> Enum.sum()
  end

  defp find_spelled_pairs(string) do
    first = rec_find_first(string)
    last = string |> String.reverse() |> rec_find_first()

    String.to_integer(first <> last)
  end

  @numbers ?1..?9
  def rec_find_first(<<n, _rest::binary>>) when n in @numbers,
    do: :unicode.characters_to_binary([n])

  def rec_find_first("one" <> _), do: "1"
  def rec_find_first("eno" <> _), do: "1"
  def rec_find_first("two" <> _), do: "2"
  def rec_find_first("owt" <> _), do: "2"
  def rec_find_first("three" <> _), do: "3"
  def rec_find_first("eerht" <> _), do: "3"
  def rec_find_first("four" <> _), do: "4"
  def rec_find_first("ruof" <> _), do: "4"
  def rec_find_first("five" <> _), do: "5"
  def rec_find_first("evif" <> _), do: "5"
  def rec_find_first("six" <> _), do: "6"
  def rec_find_first("xis" <> _), do: "6"
  def rec_find_first("seven" <> _), do: "7"
  def rec_find_first("neves" <> _), do: "7"
  def rec_find_first("eight" <> _), do: "8"
  def rec_find_first("thgie" <> _), do: "8"
  def rec_find_first("nine" <> _), do: "9"
  def rec_find_first("enin" <> _), do: "9"
  def rec_find_first(<<_, rest::binary>>), do: rec_find_first(rest)
end
