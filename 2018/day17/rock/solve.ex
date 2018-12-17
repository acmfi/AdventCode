defmodule Solve do
  defstruct [:map, :min_y, :max_y, :min_x, :max_x, fall: []]

  def ex(path \\ "input_test") do
    input = path |> File.read!() |> parse()

    ex1(input)
  end

  defp ex1(input) do
    fwater = [{500, input.min_y - 1}]
    input = %{input | fall: fwater} |> print_map()

    result = input |> simulate() |> print_map()

    s1 =
      result.map
      |> Map.values()
      |> Stream.filter(fn x -> x != "#" end)
      |> Enum.count()

    IO.puts("STAR1: #{inspect(s1)}")

    s2 = result.map |> Map.values() |> Stream.filter(fn x -> x == "~" end) |> Enum.count()

    IO.puts("STAR2: #{inspect(s2)}")
  end

  defp simulate(%{fall: []} = result), do: result

  defp simulate(%{fall: [{_, y} | rest], max_y: max_y} = result) when y > max_y,
    do: simulate(%{result | fall: rest})

  defp simulate(%{map: map, fall: fallpos, max_y: max_y} = g) do
    {map, to_fill} = fall(fallpos, map, max_y)

    to_fill = MapSet.new(to_fill) |> Enum.to_list()

    {map, to_fall} = fill(to_fill, map)
    to_fall = MapSet.new(to_fall) |> Enum.to_list()

    g = %{g | map: map, fall: to_fall}

    simulate(g)
  end

  defp fall(falls, map, max_y) when is_list(falls) do
    Enum.reduce(falls, {map, []}, fn fpos, {map, fill} ->
      {map, nfill} = fall(fpos, map, max_y)
      {map, fill ++ nfill}
    end)
  end

  defp fall({_x, y}, map, max_y) when y >= max_y, do: {map, []}

  defp fall({x, y} = pos, map, max_y) do
    npos = {x, y + 1}

    # Fall until # is found
    case map[npos] do
      "#" ->
        {map, [pos]}

      _ ->
        map = Map.put(map, npos, "|")
        fall(npos, map, max_y)
    end
  end

  defp fill(to_fill, map), do: fill(to_fill, map, [])
  defp fill([], map, to_fall), do: {map, to_fall}

  defp fill([{x, y} = pos | rest], map, to_fall) do
    case fill_h(map, pos) do
      {map, {[], []}} ->
        # If we are not falling left or right, let's fill the upper column
        up = {x, y - 1}
        fill(rest ++ [up], map, to_fall)

      {map, {fl, fr}} ->
        # If somewhere to fall, keep doing it!
        to_fall = to_fall ++ fl ++ fr
        fill(rest, map, to_fall)
    end
  end

  defp fill_h(map, pos) do
    t = MapSet.new()
    {leftf, t} = fill_h(map, pos, -1, t)
    {rightf, t} = fill_h(map, pos, +1, t)

    # If won't fall left or right, it means this is stalled water
    fill_or_fall =
      case leftf == [] and rightf == [] do
        true ->
          Enum.map(t, &{&1, "~"})

        _ ->
          Enum.map(t, &{&1, "|"})
      end

    map = Map.merge(map, Enum.into(fill_or_fall, %{}))

    {map, {leftf, rightf}}
  end

  defp fill_h(map, {x, y} = pos, offset, t) do
    case map[pos] do
      "#" ->
        # We are a block
        {[], t}

      _ ->
        t = MapSet.put(t, pos)

        case map[{x, y + 1}] do
          x when x in [nil, "|"] ->
            # If this next position is nil or falling water, we have to fall in this position
            {[pos], t}

          _ ->
            # Either way, keep growing horizontally
            fill_h(map, {x + offset, y}, offset, t)
        end
    end
  end

  defp parse(t) do
    map = t |> String.split("\n") |> Stream.flat_map(&parse_line/1) |> Enum.into(%{})

    {{min_x, max_x}, {min_y, max_y}} =
      map
      |> Map.keys()
      |> Enum.reduce({{999_999, 0}, {999_999, 0}}, fn {x, y}, {{min_x, max_x}, {min_y, max_y}} ->
        {{min(min_x, x), max(max_x, x)}, {min(min_y, y), max(max_y, y)}}
      end)

    %__MODULE__{map: map, min_y: min_y, max_y: max_y, min_x: min_x, max_x: max_x}
  end

  @line_r ~r/(x|y)=(\d+), (x|y)=(\d+)\.\.(\d+)/
  defp parse_line(line) do
    [_, static_pos, static_n, _range_pos, range_n1, range_n2] = Regex.run(@line_r, line)

    st = parse_int(static_n)

    Enum.map(Range.new(parse_int(range_n1), parse_int(range_n2)), fn r ->
      {create_coord(static_pos, st, r), "#"}
    end)
  end

  defp create_coord("x", x, y), do: {x, y}
  defp create_coord("y", y, x), do: {x, y}

  defp parse_int(x), do: String.to_integer(x)

  defp print_map(%{map: map, min_x: minx, max_x: maxx, min_y: miny, max_y: maxy} = r) do
    IO.puts("\n\n\n-----------\n\n\n")

    Enum.each(Range.new(miny, maxy), fn y ->
      Enum.map(Range.new(minx, maxx), fn x ->
        pos = {x, y}

        case map[pos] do
          nil -> "."
          o -> o
        end
      end)
      |> Enum.join("")
      |> IO.puts()
    end)

    r
  end
end
