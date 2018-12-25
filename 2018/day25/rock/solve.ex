defmodule Solve do
  def ex(path \\ "input_test") do
    input = path |> File.read!() |> parse!()
    s1 = input |> ex1()
    IO.puts("Star 1: #{Enum.count(s1)}")
  end

  defp ex1(input) do
    find_distances(input) |> join_constellations()
  end

  defp find_distances(stars) do
    Enum.reduce(stars, %{}, fn s, cons ->
      v =
        Stream.map(stars, fn s2 ->
          d = dist(s, s2)
          {s2, d}
        end)
        |> Enum.into(%{})

      Map.put(cons, s, v)
    end)
  end

  defp join_constellations(cons, res \\ %{}) do
    case Enum.count(cons) do
      0 ->
        res

      _ ->
        {k, v} = Enum.at(cons, 0)
        cons = Map.delete(cons, k)
        v = v |> Enum.filter(&at_cons/1) |> Enum.into(%{})
        {c, cons} = merge_cons(v, cons)
        join_constellations(cons, Map.put(res, k, c))
    end
  end

  defp merge_cons(v, pcons) do
    {nexts, cons} = Map.split(pcons, Map.keys(v))

    if nexts == %{} do
      {v, cons}
    else
      ns =
        nexts
        |> Map.values()
        |> Enum.map(fn x -> x |> Enum.filter(&at_cons/1) |> Enum.into(%{}) end)
        |> Enum.reduce(%{}, &Map.merge/2)

      v = Map.merge(v, ns)
      merge_cons(v, cons)
    end
  end

  defp at_cons({_, d}), do: d <= 3

  defp dist(p1, p2) do
    p1 |> Enum.zip(p2) |> Enum.map(fn {x, y} -> abs(x - y) end) |> Enum.sum()
  end

  # defp dist({x, y, z, a}, {x2, y2, z2, a2}) do
  #   abs(x - x2) + abs(y - y2) + abs(z - z2) + abs(a - a2)
  # end

  defp parse!(t) do
    t
    |> String.split("\n")
    |> Enum.map(fn l ->
      l |> String.split(",") |> Enum.map(&String.to_integer/1)
    end)
  end
end
