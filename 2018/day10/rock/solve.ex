defmodule Solve do
  def ex(path \\ "input_test") do
    path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_point/1)
    |> find_iteration()
    |> draw()
  end

  defp draw({iteration, points}) do
    {xs, ys} = points_xs_ys(points)
    min_x = Enum.min(xs)
    max_x = Enum.max(xs)
    max_y = Enum.max(ys)
    min_y = Enum.min(ys)

    points_set = points_to_set(points)

    IO.puts("Graph iteration #{iteration} of X: #{min_x},#{max_x} to Y: #{min_y},#{max_y}")

    Enum.each(Range.new(min_y, max_y), fn i ->
      line =
        Enum.map(Range.new(min_x, max_x), fn j ->
          case MapSet.member?(points_set, {j, i}) do
            true -> "#"
            _ -> "."
          end
        end)

      IO.puts(Enum.join(line, ""))
    end)
  end

  defp find_iteration(points, iteration \\ 0, last_bound \\ nil)

  defp find_iteration(points, iteration, last_bound) do
    new_points = move_points(points)
    new_bound = bound(new_points)

    case greater_bound(new_bound, last_bound) do
      true ->
        find_iteration(new_points, iteration + 1, new_bound)

      _ ->
        {iteration, points}
    end
  end

  defp move_points(points), do: move_points(points, [])

  defp move_points([], points), do: Enum.reverse(points)

  defp move_points([{{x, y}, {vx, vy}} | rest], points) do
    new_point = {{x + vx, y + vy}, {vx, vy}}
    move_points(rest, [new_point | points])
  end

  defp points_to_set(points) do
    points |> Enum.map(fn {{x, y}, _} -> {x, y} end) |> MapSet.new()
  end

  defp greater_bound(_new, nil), do: true
  defp greater_bound(new, last), do: new < last

  defp bound(points) do
    {xs, ys} = points_xs_ys(points)

    Enum.max(xs) - Enum.min(xs) + (Enum.max(ys) - Enum.min(ys))
  end

  defp points_xs_ys(points) do
    Enum.reduce(points, {[], []}, fn {{x, y}, _}, {xs, ys} ->
      {[x | xs], [y | ys]}
    end)
  end

  @reg ~r/position=<((?:[ -])?\d+), ((?:[ -])?\d+)> velocity=<((?:[ -])?\d+), ((?:[ -])?\d+)>/
  defp parse_point(line) do
    [_, x, y, vx, vy] = Regex.run(@reg, line)

    {{to_int(x), to_int(y)}, {to_int(vx), to_int(vy)}}
  end

  defp to_int(x), do: x |> String.trim() |> Integer.parse() |> elem(0)
end
