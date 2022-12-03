defmodule Cuwano do
  def test(star) do
    parse_and_solve("test_input", star)
  end

  def solve(star) do
    parse_and_solve("input", star)
  end

  defp parse_and_solve(file_name, star) do
    solve = if star in [:star1, 1], do: &star1/1, else: &star2/1

    file_name |> parse_input() |> solve.() |> IO.inspect()
  end

  defp parse_input(file) do
    file |> File.read!() |> parse()
  end

  def parse(text) do
    String.split(text, "\n")
  end

  @letters Enum.flat_map([?a..?z, ?A..?Z], fn range -> Enum.map(range, &<<&1::utf8>>) end)
  @numbers 1..52
  @letter_to_number @letters |> Enum.zip(@numbers) |> Enum.into(%{})

  def star1(input) do
    input
    |> Enum.map(&get_repeated_letter/1)
    |> Enum.map(&@letter_to_number[&1])
    |> Enum.sum()
  end

  defp get_repeated_letter(rucksack) do
    at = trunc(String.length(rucksack) / 2)
    {first, second} = String.split_at(rucksack, at)

    first = first |> String.graphemes() |> MapSet.new()
    second = second |> String.graphemes() |> MapSet.new()

    first
    |> MapSet.intersection(second)
    |> MapSet.to_list()
    |> List.first()
  end

  def star2(input) do
    input
    |> Enum.chunk_every(3)
    |> Enum.map(&get_item_type_number/1)
    |> Enum.sum()
  end

  defp get_item_type_number(rucksacks) do
    [first, second, third] = Enum.map(rucksacks, &rucksack_to_mapset/1)

    letter =
      first
      |> MapSet.intersection(second)
      |> MapSet.intersection(third)
      |> MapSet.to_list()
      |> List.first()

    @letter_to_number[letter]
  end

  defp rucksack_to_mapset(rucksack), do: rucksack |> String.graphemes() |> MapSet.new()
end
