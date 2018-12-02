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

  def solve2(path \\ "./input") do
    case File.read!(path) |> to_charlist() |> :parser.string() do
      {:ok, tokens, _} ->
        {l1, l2} = tokens |> find_pair()
        Logger.info("#{inspect(l1)}, #{inspect(l2)}")
        equalize(l1, l2)

      {:error, {error_line, :lexer, {:illegal, symbol}}, _} ->
        Logger.error("Invalid symbol #{symbol} in line #{error_line}")
        {:error, {:invalid_symbol, {error_line, symbol}}}
    end
  end

  def diff(list1, list2) do
    diff(list1, list2, 0)
  end

  def diff([], [], res), do: res

  def diff([h | list1], [h | list2], res) do
    diff(list1, list2, res)
  end

  def diff([_ | list1], [_ | list2], res) do
    diff(list1, list2, res + 1)
  end

  def check_diff(id, ids) do
    case Enum.find(ids, fn x -> diff(id, x) == 1 end) do
      nil -> nil
      id2 -> {id, id2}
    end
  end

  def find_pair(all) do
    Enum.map(all, fn x -> check_diff(x, all) end) |> Enum.find(fn x -> x != nil end)
  end

  def equalize(l1, l2) do
    equalize(l1, l2, [])
  end

  def equalize([], _, result), do: result |> Enum.reverse()

  def equalize([h | rest], [h | rest2], result) do
    equalize(rest, rest2, [h | result])
  end

  def equalize([_ | rest], [_ | rest2], result) do
    equalize(rest, rest2, result)
  end
end
