defmodule Solve do
  def ex(path \\ "input_test") do
    game = path |> File.read!() |> parse()

    s1 = ex1(game)
    IO.puts("Star 1: #{s1}")

    s2 = ex2(game)
    IO.puts("Star 2: #{s2}")
  end

  defp ex1(game) do
    game
    |> play!(10)
    |> magic_number()
  end

  @turns 1_000_000_000
  defp ex2(game) do
    {cycle, fixed} =
      Enum.reduce_while(Range.new(0, @turns), {game, %{}}, fn i, {game, prevs} ->
        {_, x, y} = game
        ng = play!(game, 1)

        case prevs[ng] do
          nil ->
            nexts = Map.put(prevs, ng, i)
            {:cont, {{ng, x, y}, nexts}}

          prev ->
            cycle = i - prev
            {:halt, {cycle, i}}
        end
      end)

    pattern = rem(@turns - fixed, cycle)
    similar_count = fixed + pattern - cycle

    IO.puts("Cycle: #{cycle}, Similar iteration: #{similar_count}, let's calculate it...")

    game |> play!(similar_count) |> magic_number() |> IO.inspect()
  end

  defp magic_number(game) do
    grouped =
      game
      |> Map.values()
      |> Stream.filter(fn x -> x != :open end)
      |> Enum.group_by(& &1)

    wooded = grouped[:tree] |> Enum.count()
    lumbs = grouped[:lumb] |> Enum.count()

    wooded * lumbs
  end

  defp play!({game, _, _}, 0), do: game

  defp play!({game, x, y}, ticks) do
    game = turn(game, x, y)

    play!({game, x, y}, ticks - 1)
  end

  defp turn(game, x, y) do
    Enum.reduce(Range.new(0, x), %{}, fn x, ng ->
      Enum.reduce(Range.new(0, y), ng, fn y, ng ->
        p = {x, y}
        kind = new_kind(p, game)
        Map.put(ng, p, kind)
      end)
    end)
  end

  defp new_kind(point, game) do
    case game[point] do
      :open ->
        game |> adjacents(point) |> at_least(3, :tree) |> transform(:tree, :open)

      :tree ->
        game |> adjacents(point) |> at_least(3, :lumb) |> transform(:lumb, :tree)

      :lumb ->
        adjs = game |> adjacents(point)
        c1 = at_least(adjs, 1, :lumb)
        c2 = at_least(adjs, 1, :tree)
        transform(c1 and c2, :lumb, :open)
    end
  end

  defp transform(true, t, _), do: t
  defp transform(false, _, f), do: f

  def at_least(adjs, min, check, count \\ 0)

  def at_least(_, count, _check, count), do: true
  def at_least([], min, _check, count), do: min <= count

  def at_least([kind | rest], min, kind, count), do: at_least(rest, min, kind, count + 1)
  def at_least([_ | rest], min, kind, count), do: at_least(rest, min, kind, count)

  defp adjacents(game, {x, y}) do
    [
      {x - 1, y - 1},
      {x, y - 1},
      {x + 1, y - 1},
      {x - 1, y},
      {x + 1, y},
      {x - 1, y + 1},
      {x, y + 1},
      {x + 1, y + 1}
    ]
    |> Stream.map(&Map.get(game, &1))
    |> Stream.filter(&(not is_nil(&1)))
    |> Enum.to_list()
  end

  defp parse(text) do
    map =
      text
      |> String.split("\n", strip: true)
      |> Stream.with_index()
      |> Enum.reduce(%{}, fn {line, y}, acc ->
        line
        |> String.codepoints()
        |> Stream.with_index()
        |> Enum.reduce(acc, fn {char, x}, acc ->
          c =
            case char do
              "." -> :open
              "|" -> :tree
              "#" -> :lumb
            end

          Map.put(acc, {x, y}, c)
        end)
      end)

    {mx, my} =
      map
      |> Map.keys()
      |> Enum.reduce({0, 0}, fn {x, y}, {mx, my} -> {max(x, mx), max(y, my)} end)

    {map, mx, my}
  end
end
