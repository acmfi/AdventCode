defmodule Ironjanowar do
  require Logger

  def format_input(id: id, position: position, size: size) do
    [px, py] = position |> List.to_string() |> String.split(",") |> Enum.map(&String.to_integer/1)
    [sx, sy] = size |> List.to_string() |> String.split("x") |> Enum.map(&String.to_integer/1)
    id = id |> List.to_string() |> String.slice(1..-1) |> String.to_integer()

    [id: id, position: {px, py}, size: {sx, sy}]
  end

  def calculate_points(id: _id, position: {px, py}, size: {sx, sy}) do
    points =
      for x <- px..(px + sx - 1) do
        for y <- py..(py + sy - 1), do: {x, y}
      end

    List.flatten(points)
  end

  def get_duplicates(all) do
    {r, _} =
      all
      |> List.foldl({%{}, %{}}, fn x, {result, checked} ->
        case Map.get(checked, x) do
          nil -> {result, Map.put(checked, x, 0)}
          _ -> {Map.put(result, x, 0), checked}
        end
      end)

    r
  end

  def solve1(path \\ "./input") do
    case File.read!(path) |> to_charlist() |> :lex.string() do
      {:ok, tokens, _} ->
        tokens
        |> Enum.chunk_every(3)
        |> Enum.map(&format_input/1)
        |> Enum.map(&calculate_points/1)
        |> List.flatten()
        |> get_duplicates()
        |> Map.keys()
        |> Enum.count()

      {:error, {error_line, :lexer, {:illegal, symbol}}, _} ->
        Logger.error("Invalid symbol #{symbol} in line #{error_line}")
        {:error, {:invalid_symbol, {error_line, symbol}}}
    end
  end
end
