defmodule Aoc.Aoc2020.Day20.Solve do
  @base_path Path.dirname(__ENV__.file)

  @monster ["..................#.", "#....##....##....###", ".#..#..#..#..#..#..."]

  @monster_pos Enum.with_index(@monster)
               |> Enum.flat_map(fn {l, i} ->
                 String.codepoints(l)
                 |> Enum.with_index()
                 |> Enum.filter(&(elem(&1, 0) == "#"))
                 |> Enum.map(&elem(&1, 1))
                 |> Enum.map(&{i, &1})
               end)

  @monster_length Enum.count(@monster_pos)

  def star1(input) do
    input
    |> build_graph()
    |> corners()
    |> Enum.map(&String.to_integer/1)
    |> Enum.reduce(&*/2)
  end

  def star2(input) do
    g = build_graph(input)

    map = build_map(g)

    monsters = find_monsters(map)

    map_size(map) - monsters * @monster_length
  end

  defp build_graph(input) do
    {g, edges} =
      Enum.reduce(input, {Graph.new(), %{}}, fn {tile, n}, {g, edges} ->
        g = Graph.add_vertex(g, n, [tile])

        edges =
          edges(tile)
          |> Enum.reduce(edges, fn edge, edges ->
            Map.update(edges, edge, [n], &(&1 ++ [n]))
            |> Map.update(String.reverse(edge), [n], &(&1 ++ [n]))
          end)

        {g, edges}
      end)

    Enum.reduce(edges, g, fn
      {_, [_]}, g -> g
      {e, [t1, t2]}, g -> Graph.add_edge(g, t1, t2, label: [e, String.reverse(e)])
    end)
  end

  defp corners(g) do
    g
    |> Graph.vertices()
    |> Enum.filter(fn v ->
      length(Graph.neighbors(g, v)) == 2
    end)
  end

  defp find_monsters(map) do
    Enum.reduce_while(rotation_combs(), 0, fn {r, f}, _ ->
      map = rotate(map, r) |> maybe_flip(f)

      case n_monsters(map) do
        0 -> {:cont, 0}
        n -> {:halt, n}
      end
    end)
  end

  defp n_monsters(map) do
    max = max_tile(map)

    max_i = max - 2
    max_j = max - 19

    for inc_i <- 0..max_i,
        inc_j <- 0..max_j,
        monster_pos = @monster_pos |> Enum.map(fn {i, j} -> {i + inc_i, j + inc_j} end),
        Enum.all?(monster_pos, &(Map.get(map, &1, ".") == "#")) do
      1
    end
    |> Enum.sum()
  end

  defp build_map(g) do
    {tiles_map, g} = build_tiles_map_oriented(g)

    build_map_from_tiles(g, tiles_map)
  end

  defp build_map_from_tiles(g, tiles_map) do
    for {tile_id, {mi, mj}} <- tiles_map,
        i <- 1..8,
        j <- 1..8,
        tile = get_tile(g, tile_id),
        Map.has_key?(tile, {i, j}),
        into: %{} do
      mi = 8 * mi
      mj = 8 * mj
      pos = {mi + i - 1, mj + j - 1}
      v = Map.get(tile, {i, j})
      {pos, v}
    end
  end

  defp build_tiles_map_oriented(g) do
    corner = corners(g) |> Enum.at(0)

    g = rotate_top_left(g, corner)

    map = %{corner => {0, 0}}

    Enum.reduce(traverse_edges(g, corner), {map, g}, fn x, {map, g} ->
      {n1, n2} =
        cond do
          Map.has_key?(map, x.v1) -> {x.v1, x.v2}
          Map.has_key?(map, x.v2) -> {x.v2, x.v1}
          true -> raise "Something wrong happened"
        end

      edges = x.label

      tile1 = get_tile(g, n1)
      tile2 = get_tile(g, n2)

      tile_edges = edges(tile1)

      edge_i =
        0..3
        |> Enum.find(fn i ->
          Enum.at(tile_edges, i) in edges
        end)

      n2_pos = sum_position(Map.get(map, n1), edge_i)

      map = Map.put(map, n2, n2_pos)

      g = orient_tile(g, tile2, n2, tile1, edge_i)

      {map, g}
    end)
  end

  # Stream to be able to traverse the graph edges
  def traverse_edges(g, point) do
    Stream.resource(
      fn -> {[point], MapSet.new(), MapSet.new()} end,
      fn
        {[], _, _} ->
          {:halt, :ok}

        {[point | points], visited, visited_verts} ->
          visited = MapSet.put(visited, point)

          all_neighs =
            (points ++ Graph.neighbors(g, point))
            |> Enum.uniq()
            |> Enum.filter(&(not MapSet.member?(visited, &1)))

          verts =
            Graph.edges(g, point)
            |> Enum.uniq()
            |> Enum.filter(&(not MapSet.member?(visited_verts, &1)))

          visited_verts = MapSet.union(visited_verts, MapSet.new(verts))

          {verts, {all_neighs, visited, visited_verts}}
      end,
      fn _ -> :ok end
    )
  end

  # Find the right orientation for the tile with other tile wall of reference
  defp orient_tile(g, tile, tile_id, ref_tile, ref_tile_i) do
    ref_edge = edges(ref_tile) |> Enum.at(ref_tile_i)
    tile_edge_i = Integer.mod(ref_tile_i + 2, 4)

    Enum.reduce_while(rotation_combs(), g, fn {r, f}, g ->
      tile = rotate(tile, r) |> maybe_flip(f)

      if Enum.at(edges(tile), tile_edge_i) == ref_edge do
        g = Graph.label_vertex(g, tile_id, [tile])

        {:halt, g}
      else
        {:cont, g}
      end
    end)
  end

  defp rotation_combs, do: for(r <- 0..3, f <- [false, true], do: {r, f})

  defp sum_position({i, j}, 0), do: {i - 1, j}
  defp sum_position({i, j}, 1), do: {i, j + 1}
  defp sum_position({i, j}, 2), do: {i + 1, j}
  defp sum_position({i, j}, 3), do: {i, j - 1}

  defp rotate_top_left(g, corner) do
    tile = get_tile(g, corner)

    int_edges = Graph.edges(g, corner) |> Enum.flat_map(& &1.label) |> Enum.uniq()

    corner_ids =
      edges(tile)
      |> Enum.with_index()
      |> Enum.filter(fn {row, _} ->
        row in int_edges
      end)
      |> Enum.map(&elem(&1, 1))

    case corner_ids do
      [1, 2] ->
        g

      _ ->
        tile = rotate(tile)

        Graph.label_vertex(g, corner, [tile])
        |> rotate_top_left(corner)
    end
  end

  defp get_tile(g, t) do
    Graph.vertex_labels(g, t) |> Enum.at(-1)
  end

  defp rotate(tile, 0), do: tile
  defp rotate(tile, n), do: rotate(rotate(tile), n - 1)

  defp rotate(tile) do
    max = max_tile(tile)

    for i <- 0..max,
        j <- 0..max,
        Map.has_key?(tile, {i, j}),
        into: %{} do
      pos = {j, max - i}

      {pos, Map.get(tile, {i, j})}
    end
  end

  defp maybe_flip(tile, false), do: tile

  defp maybe_flip(tile, true) do
    flip(tile)
  end

  defp flip(tile) do
    max = max_tile(tile)

    for i <- 0..max,
        j <- 0..max,
        Map.has_key?(tile, {i, j}),
        into: %{} do
      pos = {i, max - j}

      {pos, Map.get(tile, {i, j})}
    end
  end

  defp max_tile(tile), do: Map.keys(tile) |> Enum.flat_map(&Tuple.to_list/1) |> Enum.max()

  defp edges(tile) do
    [:n, :e, :s, :w]
    |> Enum.map(fn p ->
      position_inds(p)
      |> Enum.map(fn {p1, _pinv} ->
        Map.get(tile, p1, ".")
      end)
      |> Enum.join("")
    end)
  end

  defp position_inds(:n), do: 0..9 |> Enum.map(&{{0, &1}, {9, &1}})
  defp position_inds(:e), do: 0..9 |> Enum.map(&{{&1, 9}, {&1, 0}})
  defp position_inds(:s), do: 0..9 |> Enum.map(&{{9, &1}, {0, &1}})
  defp position_inds(:w), do: 0..9 |> Enum.map(&{{&1, 0}, {&1, 9}})

  def print_map(map) do
    max = Map.keys(map) |> Enum.flat_map(&Tuple.to_list/1) |> Enum.max()

    0..max
    |> Enum.map(fn i ->
      0..max
      |> Enum.map(fn j ->
        Map.get(map, {i, j}, ".")
      end)
      |> Enum.join("")
    end)
    |> Enum.join("\n")
    |> IO.puts()
  end

  defp parse_tile(line) do
    ["Tile " <> n | tiles] = String.split(line, "\n", trim: true)
    n = String.trim(n, ":")

    tile =
      Enum.with_index(tiles)
      |> Enum.reduce(%{}, fn {row, i}, acc ->
        String.codepoints(row)
        |> Enum.with_index()
        |> Enum.reduce(acc, fn
          {"#", j}, acc ->
            Map.put(acc, {i, j}, "#")

          _, acc ->
            acc
        end)
      end)

    {tile, n}
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
    |> String.split("\n\n", trim: true)
    |> Enum.map(&parse_tile/1)
  end
end
