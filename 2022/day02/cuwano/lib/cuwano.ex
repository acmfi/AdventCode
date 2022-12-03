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
    text
    |> String.split("\n")
    |> Enum.map(fn game ->
      game |> String.split(" ") |> List.to_tuple() |> translate()
    end)
  end

  @translate %{
    "A" => 1,
    "B" => 2,
    "C" => 3,
    "X" => 1,
    "Y" => 2,
    "Z" => 3
  }
  def translate({opponent, self}) do
    {@translate[opponent], @translate[self]}
  end

  def star1(input) do
    input
    |> Enum.map(&evaluate_game/1)
    |> Enum.sum()
  end

  defp evaluate_game({_, self} = round), do: self + result(round)

  @wins [{1, 2}, {2, 3}, {3, 1}]
  defp result(round) when round in @wins, do: 6
  defp result({same, same}), do: 3
  defp result(_), do: 0

  def star2(input) do
    input
    |> Enum.map(&calculate_game/1)
    |> Enum.sum()
  end

  @wins_map %{1 => 2, 2 => 3, 3 => 1}
  @losses_map %{2 => 1, 3 => 2, 1 => 3}
  defp calculate_game({opponent, 1}), do: evaluate_game({opponent, @losses_map[opponent]})
  defp calculate_game({opponent, 2}), do: evaluate_game({opponent, opponent})
  defp calculate_game({opponent, 3}), do: evaluate_game({opponent, @wins_map[opponent]})
end
