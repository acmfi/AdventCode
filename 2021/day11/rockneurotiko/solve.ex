defmodule Aoc.Aoc2021.Day11.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    {_, n} = steps(input, 100)
    n
  end

  defp steps(input, n, lighted \\ 0)
  defp steps(input, 0, lighted), do: {input, lighted}

  defp steps(input, n, lighted) do
    {input, lights} = step(input) |> reset_input_lights()

    steps(input, n - 1, lighted + length(lights))
  end

  defp reset_input_lights({input, lights}) do
    resets = Map.new(lights, fn point -> {point, 0} end)

    {Map.merge(input, resets), lights}
  end

  defp step(input, lighted \\ [], only \\ nil) do
    check_input = if not is_nil(only), do: Map.take(input, only), else: input

    check_input
    |> Enum.sort_by(fn {p, _} -> p end)
    |> Enum.reduce({input, lighted}, fn {point, _light}, {input, lighted} ->
      light = Map.get(input, point)
      new_light = light + 1
      input = Map.put(input, point, new_light)

      cond do
        point in lighted ->
          {input, lighted}

        new_light >= 10 ->
          neighs = neighs(point)

          lighted = [point | lighted]

          step(input, lighted, neighs)

        true ->
          {input, lighted}
      end
    end)
  end

  defp neighs({ox, oy}) do
    for x <- (ox - 1)..(ox + 1),
        y <- (oy - 1)..(oy + 1),
        x >= 0,
        y >= 0,
        x <= 9,
        y <= 9,
        {x, y} != {ox, oy} do
      {x, y}
    end
  end

  def star2(input) do
    step_all_flash(input)
  end

  defp step_all_flash(input) do
    step_all_flash(input, 0, Enum.sort(Map.keys(input)))
  end

  defp step_all_flash(input, n, all_points) do
    n = n + 1

    {input, lights} = step(input) |> reset_input_lights()

    if Enum.sort(lights) == all_points do
      n
    else
      step_all_flash(input, n, all_points)
    end
  end

  defp parse_line(line) do
    String.graphemes(line) |> Enum.map(&String.to_integer/1)
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
    t
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
    |> Enum.with_index()
    |> Enum.flat_map(fn {row, x} ->
      Enum.with_index(row) |> Enum.map(fn {elem, y} -> {{x, y}, elem} end)
    end)
    |> Map.new()
  end
end
