defmodule Solve do
  def ex(path \\ "input_test") do
    input = path |> File.read!() |> parse()

    s1 = ex1(input)
    IO.puts("Star 1: #{s1}")

    s2 = ex2(input)
    IO.puts("Star 2: #{inspect(s2)}")
  end

  defp ex1(input) do
    {max_pos, max_r} = Enum.max_by(input, &elem(&1, 1))

    on_range(max_pos, max_r, input)
  end

  defp ex2(input) do
    {xs, ys, zs} = split_coords(input)

    jump = calculate_jump(xs)

    find_positions(xs, ys, zs, input, jump)
  end

  defp find_positions(xs, ys, zs, bots, jump) do
    stream_pos = positions(xs, ys, zs, jump)
    state = {0, []}

    {_, [position | _]} =
      Enum.reduce(stream_pos, state, fn tpos, {n, pos} ->
        r = bots_ranges(tpos, bots, jump)

        case {r > n, r == n} do
          {true, _} ->
            {r, [tpos]}

          {_, true} ->
            {r, [tpos | pos]}

          _ ->
            {n, pos}
        end
      end)

    end_find_positions(position, bots, jump)
  end

  defp end_find_positions({x, y, z}, _bots, 1), do: abs(x) + abs(y) + abs(z)

  defp end_find_positions({x, y, z}, bots, jump) do
    xs = {x - jump, x + jump}
    ys = {y - jump, y + jump}
    zs = {z - jump, z + jump}
    jump = div(jump, 2)
    find_positions(xs, ys, zs, bots, jump)
  end

  defp calculate_jump(xs, d \\ 1)
  defp calculate_jump({lx, up}, d) when d >= up - lx, do: d
  defp calculate_jump(xs, d), do: calculate_jump(xs, d * 2)

  @minint -99_999_999_999_999_999_999
  @maxint 99_999_999_999_999_999_999
  defp split_coords(bots) do
    init = {{@maxint, @minint}, {@maxint, @minint}, {@maxint, @minint}}

    Enum.reduce(bots, init, fn {{x, y, z}, _}, {{lx, ux}, {ly, uy}, {lz, uz}} ->
      {{min(lx, x), max(ux, x)}, {min(ly, y), max(uy, y)}, {min(lz, z), max(uz, z)}}
    end)
  end

  defp bots_ranges(pos, bots, jump) do
    bots
    |> Enum.reduce(0, fn {bt, r}, t ->
      if div(dist(pos, bt) - r, jump) <= 0 do
        t + 1
      else
        t
      end
    end)
  end

  defp on_range(target_pos, target_r, bots) do
    bots
    |> Enum.reduce(0, fn {p, _r}, t ->
      case dist(target_pos, p) do
        x when x <= target_r -> t + 1
        _ -> t
      end
    end)
  end

  def positions({minx, maxx}, {miny, maxy}, {minz, maxz}, jump) do
    Stream.resource(
      fn -> {minx, miny, minz - jump} end,
      fn {x, y, z} ->
        case {z >= maxz, y >= maxy, x >= maxx} do
          {true, false, _} ->
            p = {x, y + jump, minz}
            {[p], p}

          {true, true, false} ->
            p = {x + jump, miny, minz}
            {[p], p}

          {true, true, true} ->
            {:halt, :end}

          _ ->
            p = {x, y, z + jump}
            {[p], p}
        end
      end,
      fn _ -> :ok end
    )
  end

  defp dist({x0, y0, z0}, {x1, y1, z1}) do
    abs(x0 - x1) + abs(y0 - y1) + abs(z0 - z1)
  end

  defp parse(t) do
    t
    |> String.split("\n")
    |> Stream.filter(&(&1 != ""))
    |> Stream.map(&parse_bot/1)
    |> Enum.into(%{})
  end

  defp parse_bot(line) do
    [posl, rl] = String.split(line, ", ")
    [x, y, z] = posl |> String.trim_leading("pos=<") |> String.trim(">") |> String.split(",")
    r = rl |> String.trim("r=") |> to_int()

    p = {to_int(x), to_int(y), to_int(z)}

    {p, r}
  end

  defp to_int(x), do: String.to_integer(x)
end
