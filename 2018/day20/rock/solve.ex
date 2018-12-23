defmodule Solve do
  def ex(path \\ "input_test") do
    map = path |> File.read!() |> parse() |> walk()

    s1 = ex1(map)
    IO.puts("Star 1: #{s1}")

    s2 = ex2(map)
    IO.puts("Star 2: #{s2}")
  end

  defp ex1(map) do
    map.map |> Map.values() |> Enum.max()
  end

  defp ex2(map) do
    map.map |> Map.values() |> Stream.filter(&(&1 >= 1000)) |> Enum.count()
  end

  defp walk(paths), do: walk(paths, %{map: %{}, pos: %{{0, 0} => 0}})

  defp walk(paths, state) when is_list(paths) do
    Enum.reduce(paths, state, &walk/2)
  end

  defp walk(step, state) when step in [:u, :d, :r, :l] do
    move(state, step)
  end

  defp walk({type_bif, bif}, prev_state) when is_list(bif) do
    initial =
      case type_bif do
        :opt -> prev_state
        _ -> %{prev_state | pos: %{}}
      end

    Enum.reduce(bif, initial, fn choice, state ->
      new_state = walk(choice, %{state | pos: prev_state.pos})
      merge_pos(state.pos, new_state)
    end)
  end

  defp move(%{pos: pos} = state, step) do
    new_pos =
      Stream.map(pos, fn {pos, dist} ->
        pos = next_pos(step, pos)
        {pos, dist + 1}
      end)

    merge_pos(new_pos, %{state | pos: %{}})
  end

  defp merge_pos(new_pos, state) do
    new_pos = new_pos |> Stream.map(&best_distance(&1, state)) |> Enum.into(state.pos)

    %{pos: new_pos, map: Map.merge(state.map, new_pos)}
  end

  defp best_distance({pos, dist}, state) do
    new_dist = dist |> min(Map.get(state.map, pos, dist)) |> min(Map.get(state.pos, pos, dist))
    {pos, new_dist}
  end

  defp next_pos(:u, {x, y}), do: {x, y - 1}
  defp next_pos(:d, {x, y}), do: {x, y + 1}
  defp next_pos(:r, {x, y}), do: {x + 1, y}
  defp next_pos(:l, {x, y}), do: {x - 1, y}

  defp parse("^" <> r), do: parse(r, [])
  defp parse("N" <> r, acc), do: parse(r, acc ++ [:u])
  defp parse("S" <> r, acc), do: parse(r, acc ++ [:d])
  defp parse("E" <> r, acc), do: parse(r, acc ++ [:r])
  defp parse("W" <> r, acc), do: parse(r, acc ++ [:l])

  defp parse("(" <> r, acc) do
    {r, inner} = parse(r, [])

    inner =
      case Enum.at(inner, -1) do
        :cont -> {:opt, Enum.drop(inner, -1)}
        _ -> {:all, inner}
      end

    parse(r, acc ++ [inner])
  end

  defp parse(")" <> r, acc), do: {r, [acc]}

  defp parse("|)" <> r, acc) do
    {r, [acc] ++ [:cont]}
  end

  defp parse("|" <> r, acc) do
    {r, others} = parse(r, [])
    {r, [acc] ++ others}
  end

  defp parse("$", acc), do: acc
end
