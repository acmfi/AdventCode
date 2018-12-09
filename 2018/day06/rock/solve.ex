defmodule Solve do
  def ex(path \\ "input_test") do
    vectors =
      path
      |> File.read!()
      |> String.split("\n", trim: true)
      |> to_vectors()

    sol1 = vectors |> build_matrix() |> calculate_areas_and_infinites() |> Enum.at(0) |> elem(1)

    IO.puts("Solution 1: #{inspect(sol1)}")

    sol2 = vectors |> safe_area()

    IO.puts("Solution 2: #{inspect(sol2)}")
  end

  defp to_vectors(lines) do
    {_id, m, acc} =
      Enum.reduce(lines, {1, 0, []}, fn l, {id, m, acc} ->
        [x, y] = String.split(l, ",") |> Stream.map(&String.trim/1) |> Enum.map(&to_int/1)

        new_m = max(x, max(y, m))

        new_acc = acc ++ [{id, {x, y}}]
        {id + 1, new_m, new_acc}
      end)

    {m, Enum.into(acc, %{})}
  end

  defp build_matrix({m, points}) do
    Enum.map(0..m, fn i ->
      Enum.map(0..m, fn j ->
        best_point(points, {i, j})
      end)
    end)
  end

  defp best_point(points, target) do
    points
    |> all_manhattans(target)
    |> Enum.sort_by(&elem(&1, 1))
    |> get_winner()
  end

  defp all_manhattans(points, target) do
    points
    |> Stream.map(fn {id, point} ->
      {id, manhattan(target, point)}
    end)
  end

  defp calculate_areas_and_infinites(matrix) do
    left = matrix |> Enum.at(0) |> ids_of()
    right = matrix |> Enum.at(-1) |> ids_of()
    top = matrix |> row(0) |> ids_of()
    bottom = matrix |> row(-1) |> ids_of()
    infinites = Enum.uniq(left ++ right ++ top ++ bottom)

    all_surfaces = calculate_surfaces(matrix)

    all_surfaces
    |> Stream.filter(fn {id, _} -> not Enum.member?(infinites, id) end)
    |> Stream.filter(fn {id, _} -> is_integer(id) end)
    |> Enum.sort_by(&elem(&1, 1), &>=/2)
  end

  @max_safe 10_000
  defp safe_area({m, points}) do
    Stream.flat_map(0..m, fn i ->
      Enum.map(0..m, fn j ->
        calculate_total(points, {i, j})
      end)
    end)
    |> Stream.filter(fn x -> x < @max_safe end)
    |> Enum.count()
  end

  defp calculate_total(points, target) do
    points |> all_manhattans(target) |> Stream.map(&elem(&1, 1)) |> Enum.sum()
  end

  defp calculate_surfaces(matrix) do
    Enum.reduce(matrix, %{}, fn row, acc ->
      calcs =
        Enum.group_by(row, &elem(&1, 0))
        |> Enum.map(fn {id, n_rows} -> {id, Enum.count(n_rows)} end)

      Enum.reduce(calcs, acc, fn {id, n}, acc ->
        old = acc[id] || 0
        Map.put(acc, id, n + old)
      end)
    end)
  end

  defp ids_of(row) do
    row
    |> Stream.map(fn {id, _m} -> id end)
    |> Stream.filter(&is_integer/1)
    |> Enum.uniq()
  end

  defp row(matrix, index) do
    Enum.map(matrix, &Enum.at(&1, index))
  end

  # defp get_winner([{id, 0} | _]), do: {id, 0}
  defp get_winner([{_, m}, {_, m} | _]), do: {:draw, m}
  defp get_winner([{id, m} | _]), do: {id, m}

  defp to_int(x), do: Integer.parse(x) |> elem(0)
  defp manhattan({a, b}, {c, d}), do: abs(a - c) + abs(b - d)
end
