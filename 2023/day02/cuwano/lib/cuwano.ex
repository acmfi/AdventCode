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

  def parse(text) do
    text |> String.split("\n") |> Map.new(&parse_game/1)
  end

  defp parse_game(string) do
    ["Game " <> game_id, rest] = String.split(string, ": ")

    game_id = String.to_integer(game_id)
    {game_id, parse_sets(rest)}
  end

  defp parse_sets(string) do
    string |> String.split("; ") |> Enum.map(&parse_set/1)
  end

  defp parse_set(string) do
    string |> String.split(", ") |> Enum.reduce(%{}, &parse_cube/2)
  end

  defp parse_cube(string, cubes) do
    [number, color] = String.split(string, " ")

    number = String.to_integer(number)
    Map.put(cubes, color, number)
  end

  @max_red 12
  @max_green 13
  @max_blue 14
  def star1(input) do
    input
    |> Enum.filter(fn {_, game} ->
      Enum.all?(game, fn set ->
        red = set["red"] || 0
        green = set["green"] || 0
        blue = set["blue"] || 0

        red <= @max_red and green <= @max_green and blue <= @max_blue
      end)
    end)
    |> Enum.map(&elem(&1, 0))
    |> Enum.sum()
  end

  def star2(input) do
    input |> Enum.map(&game_power/1) |> Enum.sum()
  end

  defp game_power({_, game}) do
    {red, green, blue} =
      Enum.reduce(game, {0, 0, 0}, fn set, {result_red, result_green, result_blue} ->
        red = set["red"] || 0
        green = set["green"] || 0
        blue = set["blue"] || 0

        {
          Enum.max([red, result_red]),
          Enum.max([green, result_green]),
          Enum.max([blue, result_blue])
        }
      end)

    red * green * blue
  end
end
