defmodule Aoc.Aoc2020.Day24.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    input |> turn_tiles() |> MapSet.size()
  end

  def star2(input) do
    input |> turn_tiles() |> live(100) |> MapSet.size()
  end

  defp live(tiles, 0), do: tiles

  defp live(tiles, n) do
    tiles |> step() |> live(n - 1)
  end

  defp step(tiles) do
    Enum.reduce(tiles, tiles, fn tile, ntiles ->
      ntiles = if go_white?(tile, tiles), do: MapSet.delete(ntiles, tile), else: ntiles

      Enum.reduce(new_blacks(tile, tiles), ntiles, &MapSet.put(&2, &1))
    end)
  end

  defp go_white?(tile, tiles) do
    c =
      tile
      |> neighs()
      |> Enum.map(&MapSet.member?(tiles, &1))
      |> Enum.filter(& &1)
      |> Enum.count()

    c == 0 or c > 2
  end

  defp new_blacks(tile, tiles) do
    tile
    |> neighs()
    |> Enum.filter(&(not MapSet.member?(tiles, &1)))
    |> Enum.map(&{&1, neighs(&1)})
    |> Enum.filter(fn {_p, ns} ->
      c = Enum.map(ns, &MapSet.member?(tiles, &1)) |> Enum.filter(& &1) |> Enum.count()
      c == 2
    end)
    |> Enum.map(&elem(&1, 0))
  end

  defp neighs(pos) do
    [:e, :w, :ne, :nw, :se, :sw]
    |> Enum.map(&apply_dir(&1, pos))
  end

  defp turn_tiles(input) do
    Enum.reduce(input, MapSet.new(), fn d, tiles ->
      tile = apply_dirs(d)

      case MapSet.member?(tiles, tile) do
        true -> MapSet.delete(tiles, tile)
        _ -> MapSet.put(tiles, tile)
      end
    end)
  end

  defp apply_dirs(pos \\ {0, 0, 0}, ndirs)

  defp apply_dirs(pos, ndirs) do
    Enum.reduce(ndirs, pos, &apply_dir/2)
  end

  defp apply_dir(d, {x, y, z}) do
    {nx, ny, nz} = dirs(d)
    {x + nx, y + ny, z + nz}
  end

  defp dirs(:e), do: {1, 1, 0}
  defp dirs(:w), do: {-1, -1, 0}

  defp dirs(:ne), do: {0, 1, 1}
  defp dirs(:nw), do: {-1, 0, 1}

  defp dirs(:se), do: {1, 0, -1}
  defp dirs(:sw), do: {0, -1, -1}

  defp parse_line(line, dirs \\ [])
  defp parse_line("", dirs), do: dirs

  defp parse_line("e" <> line, dirs) do
    parse_line(line, dirs ++ [:e])
  end

  defp parse_line("se" <> line, dirs) do
    parse_line(line, dirs ++ [:se])
  end

  defp parse_line("sw" <> line, dirs) do
    parse_line(line, dirs ++ [:sw])
  end

  defp parse_line("w" <> line, dirs) do
    parse_line(line, dirs ++ [:w])
  end

  defp parse_line("ne" <> line, dirs) do
    parse_line(line, dirs ++ [:ne])
  end

  defp parse_line("nw" <> line, dirs) do
    parse_line(line, dirs ++ [:nw])
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
  end
end
