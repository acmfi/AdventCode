defmodule Solve do
  def ex(input) do
    input |> ex1()
    input |> ex2()
  end

  defp ex1(input) do
    IO.puts("PART 1")
    input |> scores(10) |> Enum.join("") |> IO.puts()
  end

  defp ex2(input) do
    to_find = input |> to_string() |> String.codepoints() |> Enum.map(&String.to_integer/1)
    IO.puts("PART 2")
    find(to_find)
  end

  @initial %{1 => 3, 2 => 7}
  defp scores(first, max) do
    last = first + max

    calculate(@initial, {1, 2}, 3, first + 1, last)
  end

  defp find(to_find) do
    find(@initial, {1, 2}, 3, [3, 7], to_find)
  end

  defp find(scores, {e1, e2}, next_score, actual, to_find) do
    with {:cont, actual} <- found(actual, to_find) do
      {scores, next_score, added_scores} = new_scores(scores, e1, e2, next_score)
      {e1, e2} = new_positions(scores, e1, e2, next_score)

      actual = actual ++ added_scores

      find(scores, {e1, e2}, next_score, actual, to_find)
    else
      {:stop, x} ->
        IO.puts("#{next_score - 1 - length(to_find) - x}")
    end
  end

  defp found(actual, to_find) when actual == to_find, do: {:stop, 0}
  defp found(actual, to_find) when length(actual) < length(to_find), do: {:cont, actual}

  defp found(actual, to_find) when length(actual) == length(to_find),
    do: {:cont, Enum.drop(actual, 1)}

  defp found(actual, to_find) when length(actual) > length(to_find) do
    a = Enum.take(actual, length(to_find))

    case found(a, to_find) do
      {:stop, x} -> {:stop, x + length(actual) - length(to_find)}
      {:cont, _} -> found(Enum.drop(actual, 1), to_find)
    end
  end

  defp calculate(scores, _pos, next_score, first, last) when next_score > last,
    do: get_scores(scores, first, last)

  defp calculate(scores, {e1, e2}, next_score, first, last) do
    # IO.puts(board(scores, next_score))
    # IO.puts("#{inspect({e1, e2})} -> #{next_score}")

    {scores, next_score, _} = new_scores(scores, e1, e2, next_score)
    {e1, e2} = new_positions(scores, e1, e2, next_score)
    calculate(scores, {e1, e2}, next_score, first, last)
  end

  defp new_scores(scores, e1, e2, next_score) do
    s = scores[e1] + scores[e2]

    case {div(s, 10), rem(s, 10)} do
      {0, x} ->
        {Map.put(scores, next_score, x), next_score + 1, [x]}

      {x, y} ->
        {Map.put(scores, next_score, x) |> Map.put(next_score + 1, y), next_score + 2, [x, y]}
    end
  end

  defp new_positions(scores, e1, e2, next_score) do
    e1m = 1 + scores[e1]
    e2m = 1 + scores[e2]

    e1 = calc_next(e1, e1m, next_score)
    e2 = calc_next(e2, e2m, next_score)

    {e1, e2}
  end

  defp calc_next(e, em, next_score) do
    case rem(e + em, next_score - 1) do
      0 -> next_score - 1
      o -> o
    end
  end

  defp get_scores(scores, first, stop_i) do
    Range.new(first, stop_i) |> Enum.map(fn i -> scores[i] end)
  end

  defp board(scores, next_score) do
    Enum.map(Range.new(1, next_score - 1), fn i -> scores[i] end) |> Enum.join("")
  end
end
