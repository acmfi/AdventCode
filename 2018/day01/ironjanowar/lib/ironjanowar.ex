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
end
