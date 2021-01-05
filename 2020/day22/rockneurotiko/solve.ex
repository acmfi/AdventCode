defmodule Aoc.Aoc2020.Day22.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1({p1, p2}) do
    play(p1, p2) |> score()
  end

  defp play({_p1, []}, {p2, pc2}), do: {p2, pc2}

  defp play({p1, pc1}, {_, []}), do: {p1, pc1}

  defp play(p1, p2) do
    {p1, p2} = round(p1, p2)

    play(p1, p2)
  end

  defp round({p1, [c1 | pc1]}, {p2, [c2 | pc2]}) do
    {pc1, pc2} =
      cond do
        c1 > c2 ->
          {pc1 ++ [c1, c2], pc2}

        c2 > c1 ->
          {pc1, pc2 ++ [c2, c1]}
      end

    {{p1, pc1}, {p2, pc2}}
  end

  defp score({_, c}) do
    c
    |> Enum.reverse()
    |> Enum.with_index()
    |> Enum.map(fn {c, i} -> c * (i + 1) end)
    |> Enum.sum()
  end

  def star2({p1, p2}) do
    rec_combat(p1, p2) |> score()
  end

  defp rec_combat(p1, p2), do: rec_combat(p1, p2, %{}, %{})

  defp rec_combat({_p1, []}, {p2, pc2}, _, _), do: {p2, pc2}
  defp rec_combat({p1, pc1}, {_p2, []}, _, _), do: {p1, pc1}

  defp rec_combat({p1, pc1}, {_p2, pc2}, m1, m2)
       when is_map_key(m1, pc1) or is_map_key(m2, pc2) do
    {p1, pc1}
  end

  defp rec_combat({p1, [c1 | pc1]}, {p2, [c2 | pc2]}, m1, m2)
       when c1 <= length(pc1) and c2 <= length(pc2) do
    rc1 = Enum.take(pc1, c1)
    rc2 = Enum.take(pc2, c2)

    m1 = Map.put(m1, [c1 | pc1], true)
    m2 = Map.put(m2, [c2 | pc2], true)

    case rec_combat({p1, rc1}, {p2, rc2}) do
      {^p1, _} -> rec_combat({p1, pc1 ++ [c1, c2]}, {p2, pc2}, m1, m2)
      {^p2, _} -> rec_combat({p1, pc1}, {p2, pc2 ++ [c2, c1]}, m1, m2)
    end
  end

  defp rec_combat({p1, pc1}, {p2, pc2}, m1, m2) do
    m1 = Map.put(m1, pc1, true)
    m2 = Map.put(m2, pc2, true)

    {{p1, pc1}, {p2, pc2}} = round({p1, pc1}, {p2, pc2})

    rec_combat({p1, pc1}, {p2, pc2}, m1, m2)
  end

  defp parse_player(line) do
    ["Player " <> p | cards] = String.split(line, "\n", trim: true)

    cards = Enum.map(cards, &String.to_integer/1)

    {p, cards}
  end

  # Helpers

  def solve(fname \\ "input") do
    do_solve(fname)
  end

  def solve_test(), do: solve("input_test")

  defp do_solve(fname) do
    path = Path.join(@base_path, fname)

    input = path |> File.read!() |> parse!()
    star1 = star1(input)
    IO.puts("Star 1: " <> to_string(star1))

    star2 = star2(input)
    IO.puts("Star 2: " <> to_string(star2))
  end

  defp parse!(t) do
    [p1, p2] = String.split(t, "\n\n", trim: true)

    {parse_player(p1), parse_player(p2)}
  end
end
