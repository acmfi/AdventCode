defmodule Solve do
  @ind :index_agent

  @depth 11820
  @tx 7
  @ty 782

  def ex() do
    Agent.start_link(fn -> %{} end, name: @ind)

    s1 = ex1()
    IO.puts("Star 1: #{s1}")

    s2 = ex2()
    IO.puts("Star 2: #{inspect(s2)}")
  end

  defp get_index(v), do: Agent.get(@ind, &Map.fetch(&1, v))
  defp put_index(k, v), do: Agent.update(@ind, &Map.put(&1, k, v))

  defp ex1() do
    Range.new(0, @tx)
    |> Enum.reduce(%{}, fn x, acc ->
      Range.new(0, @ty)
      |> Enum.reduce(acc, fn y, result ->
        p = {x, y}
        index = region_index(p)
        risk = index |> erosion_level() |> region_type()

        # |> type_risk()
        Map.put(result, p, risk)
      end)
    end)
    |> Map.values()
    |> Enum.sum()
  end

  @maxx 3 * @tx
  @maxy 3 * @ty
  @inf 99_999_999_999_999_999_999_999_999

  @newheap Heap.min() |> Heap.push({0, 0, 0, 1})
  defp ex2(points \\ @newheap, dists \\ %{{0, 0, 1} => 0}, answers \\ [])

  defp ex2(heap, dists, answers) do
    with {{d, x, y, e}, heap} <- Heap.split(heap),
         {:filter, _, _, false} <-
           {:filter, heap, [d | answers], x == @tx and y == @ty and e == 1},
         {:filter2, _, _, false} <- {:filter2, heap, answers, x > @maxx or y > @maxy} do
      dist = {x, y, e}

      if Map.get(dists, dist, d) < d do
        ex2(heap, dists, answers)
      else
        {heap, dists} =
          dist
          |> neighbors()
          |> Stream.filter(fn {nx, ny, ne, nw} ->
            d + nw < Map.get(dists, {nx, ny, ne}, @inf)
          end)
          |> Enum.reduce({heap, dists}, fn {nx, ny, ne, nw}, {heap, dists} ->
            dist = d + nw
            dists = Map.put(dists, {nx, ny, ne}, dist)
            heap = Heap.push(heap, {dist, nx, ny, ne})
            {heap, dists}
          end)

        ex2(heap, dists, answers)
      end
    else
      {nil, nil} ->
        Enum.max(answers)

      {:filter, heap, answers, true} ->
        ex2(heap, dists, answers)

      {:filter2, heap, answers, true} ->
        ex2(heap, dists, answers)
    end
  end

  defp neighbors({x, y, e}) do
    [{x - 1, y}, {x + 1, y}, {x, y - 1}, {x, y + 1}]
    |> Stream.filter(fn {nx, ny} ->
      nx >= 0 and ny >= 0
    end)
    |> Stream.flat_map(fn {nx, ny} ->
      r = {nx, ny} |> region_index() |> erosion_level() |> region_type()

      # Try the three types
      Range.new(0, 2)
      |> Stream.filter(&(&1 != r))
      |> Stream.map(fn ne ->
        p = if e != ne, do: 8, else: 1

        {nx, ny, ne, p}
      end)
    end)
  end

  @ymult 16807
  @xmult 48271

  defp region_index({0, 0}), do: 0
  defp region_index({@tx, @ty}), do: 0
  defp region_index({0, y}), do: y * @xmult
  defp region_index({x, 0}), do: x * @ymult

  defp region_index({x, y}) do
    case get_index({x, y}) do
      {:ok, ind} ->
        ind

      _ ->
        p1 = {x - 1, y}
        p2 = {x, y - 1}
        p1_index = region_index(p1)
        p2_index = region_index(p2)

        p1_erosion = erosion_level(p1_index)
        p2_erosion = erosion_level(p2_index)

        ind = p1_erosion * p2_erosion

        put_index({x, y}, ind)
        ind
    end
  end

  @erosionmod 20183
  defp erosion_level(ind) do
    rem(ind + @depth, @erosionmod)
  end

  defp region_type(lvl) do
    rem(lvl, 3)
  end
end
