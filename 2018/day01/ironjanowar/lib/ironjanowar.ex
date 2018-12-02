defmodule Ironjanowar do
  require Logger

  def solve1(path) do
    case File.read!(path) |> to_charlist() |> :parser.string() do
      {:ok, tokens, _} ->
        tokens |> Enum.reduce(0, &+/2)

      {:error, {error_line, :lexer, {:illegal, symbol}}, _} ->
        Logger.error("Invalid symbol #{symbol} in line #{error_line}")
        {:error, {:invalid_symbol, {error_line, symbol}}}
    end
  end

  def solve2(path) do
    case File.read!(path) |> to_charlist() |> :parser.string() do
      {:ok, tokens, _} ->
        tokens |> find_duplicate(0, [], tokens)

      {:error, {error_line, :lexer, {:illegal, symbol}}, _} ->
        Logger.error("Invalid symbol #{symbol} in line #{error_line}")
        {:error, {:invalid_symbol, {error_line, symbol}}}
    end
  end

  defp find_duplicate([], current_frequency, previous_frequencies, all_frequencies),
    do: find_duplicate(all_frequencies, current_frequency, previous_frequencies, all_frequencies)

  defp find_duplicate([h | tail], current_frequency, previous_frequencies, all_frequencies) do
    new_frequency = h + current_frequency

    case new_frequency in previous_frequencies do
      true ->
        new_frequency

      false ->
        find_duplicate(
          tail,
          new_frequency,
          [new_frequency | previous_frequencies],
          all_frequencies
        )
    end
  end
end
