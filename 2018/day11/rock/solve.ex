defmodule Solve do
  def ex(path \\ "input_test") do
    path |> File.read!() |> Integer.parse() |> elem(0) |> find_best_coordinate(300, 300)
  end

  defp find_best_coordinate(serial, max_x, max_y) do
    cells = power_cells(max_x, max_y, serial)

    ex1(cells, max_x, max_y)

    ex2(cells, max_x, max_y)
  end

  defp ex1(cells, max_x, max_y) do
    powers = total_power_cells(max_x, max_y, cells)

    result = powers |> Enum.sort_by(&elem(&1, 1), &>=/2) |> Enum.at(0)
    IO.puts("EX1: #{inspect(result)}")
  end

  defp ex2(cells, max_x, max_y) do
    cs = cumulative_sums(cells, max_x, max_y)

    {_, result} =
      Enum.reduce(Range.new(1, max_x), {-100, nil}, fn i, acc ->
        Enum.reduce(Range.new(1, max_y), acc, fn j, acc ->
          t = max_x - max(i, j)

          Enum.reduce(Range.new(0, t), acc, fn s, {m, mxy} ->
            k = cs[{i + s, j + s}] + cs[{i, j}] - cs[{i + s, j}] - cs[{i, j + s}]

            if k > m, do: {k, {i + 1, j + 1, s}}, else: {m, mxy}
          end)
        end)
      end)

    IO.puts("EX2: #{inspect(result)}")
  end

  defp sum_power(x, y, size, cells) do
    max_x = min(300, x + size - 1)
    max_y = min(300, y + size - 1)

    Enum.reduce(Range.new(x, max_x), 0, fn i, acc ->
      r =
        Enum.reduce(Range.new(y, max_y), 0, fn j, acc ->
          cells[{i, j}] + acc
        end)

      acc + r
    end)
  end

  defp cumulative_sums(cells, max_x, max_y) do
    Enum.reduce(Range.new(1, max_x), %{}, fn i, cs ->
      Enum.reduce(Range.new(1, max_y), cs, fn j, cs ->
        cell = {i, j}

        t =
          cells[cell] + Map.get(cs, {i - 1, j}, 0) + Map.get(cs, {i, j - 1}, 0) -
            Map.get(cs, {i - 1, j - 1}, 0)

        Map.put(cs, cell, t)
      end)
    end)
  end

  defp total_power_cells(max_x, max_y, cells) do
    Enum.flat_map(Range.new(1, max_x - 2), fn x ->
      Enum.map(Range.new(1, max_y - 2), fn y ->
        cell = {x, y}

        # sum_power(x, y, 3, cells)
        # total_power = find_best_power_matrix(x, y, cells)
        total_power = sum_power(x, y, 3, cells)
        {cell, total_power}
      end)
    end)
    |> Enum.into(%{})
  end

  defp power_cells(max_x, max_y, serial) do
    Enum.flat_map(Range.new(1, max_x), fn x ->
      Enum.map(Range.new(1, max_y), fn y ->
        cell = {x, y}
        {cell, cell_power(cell, serial)}
      end)
    end)
    |> Enum.into(%{})
  end

  def cell_power({x, y}, serial) do
    id = x + 10
    full = (id * y + serial) * id
    hundreds(full) - 5
  end

  defp hundreds(x), do: rem(div(x, 100), 10)
end
