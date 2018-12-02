defmodule Ironjanowar do
  require Logger

  def solve1(path \\ "./input") do
    case File.read!(path) |> to_charlist() |> :parser.string() do
      {:ok, tokens, _} ->
        {doubles, triples} =
          tokens
          |> Enum.map(&get_doubles_and_triples/1)
          |> Enum.reduce({0, 0}, fn {d, t}, {ad, at} -> {d + ad, t + at} end)

        Logger.info("#{doubles}, #{triples}")

        doubles * triples

      {:error, {error_line, :lexer, {:illegal, symbol}}, _} ->
        Logger.error("Invalid symbol #{symbol} in line #{error_line}")
        {:error, {:invalid_symbol, {error_line, symbol}}}
    end
  end

  def get_doubles_and_triples(id),
    do: get_doubles_and_triples(id, [], {0, 0})

  def get_doubles_and_triples([], _skip, result), do: result

  def get_doubles_and_triples([h | rest], skip, {doubles, triples} = res) do
    case h in skip do
      true ->
        get_doubles_and_triples(rest, skip, res)

      false ->
        result =
          case Enum.count(rest, fn x -> x == h end) do
            1 -> {1, triples}
            2 -> {doubles, 1}
            _ -> {doubles, triples}
          end

        get_doubles_and_triples(rest, [h | skip], result)
    end
  end
end
