defmodule Solve do
  import Access, only: [key: 1]

  defmodule Unit do
    defstruct kind: nil, points: 200, power: 3, position: nil
  end

  defmodule Game do
    defstruct units: %{}, map: %{}, round: 0, end: false, elf_dead: false, all_or_lose: false
  end

  def ex(path \\ "input_test") do
    game = path |> File.read!() |> parse_game()
    part1 = play!(game)
    IO.puts("PART1: #{part1}")

    part2 = all_elfs_alive(game)
    IO.puts("PART2: #{part2}")
  end

  defp all_elfs_alive(game, power \\ 4) do
    units =
      Enum.map(game.units, fn {id, unit} ->
        u =
          if unit.kind == "E" do
            %{unit | power: power}
          else
            unit
          end

        {id, u}
      end)
      |> Enum.into(%{})

    elfs_game = %{game | units: units, all_or_lose: true}

    case play!(elfs_game) do
      {:error, :elf_dead} -> all_elfs_alive(game, power + 1)
      result -> result
    end
  end

  defp play!(%{all_or_lose: true, elf_dead: true}), do: {:error, :elf_dead}

  defp play!(%{end: true} = game) do
    forces = Enum.map(game.units, fn {_, u} -> u.points end) |> Enum.sum()
    (game.round - 1) * forces
  end

  defp play!(game) do
    # print_game(game)

    game = game.units |> Enum.sort_by(fn {_, u} -> u.position end) |> Enum.reduce(game, &turn/2)

    game = update_in(game, [key(:round)], &(&1 + 1))

    play!(game)
  end

  defp turn({id, _unit}, game) do
    with unit when not is_nil(unit) <- Map.get(game.units, id),
         targets when length(targets) > 0 <-
           Enum.filter(game.units, fn {_tid, tunit} -> unit.kind != tunit.kind end) do
      in_range = range_units(id, targets, game)

      unit =
        if not MapSet.member?(in_range, unit.position) do
          move_unit(unit, in_range, game)
        else
          unit
        end

      # Add new unit, and attack!
      game |> update_in([key(:units), id], fn _ -> unit end) |> attack!(unit, targets)
    else
      nil ->
        # Already dead!
        game

      [] ->
        # No targets! END!
        update_in(game, [key(:end)], fn _ -> true end)
    end
  end

  defp attack!(game, unit, targets) do
    cross_attack = cross(unit.position)

    opponents =
      targets
      |> Enum.filter(fn {_tid, tunit} -> tunit.position in cross_attack end)
      |> Enum.sort_by(fn {_id, unit} -> {unit.points, unit.position} end)

    case opponents do
      [] ->
        game

      [{tid, tunit} | _] ->
        new_points = tunit.points - unit.power

        hit_unit(game, tid, tunit, new_points)
    end
  end

  defp hit_unit(game, tid, tunit, new_points) when new_points <= 0 do
    elf_dead = tunit.kind == "E"

    # DEAD, remove unit
    game
    |> update_in([key(:units)], fn units -> Map.delete(units, tid) end)
    |> update_in([key(:elf_dead)], fn d -> d or elf_dead end)
  end

  defp hit_unit(game, tid, _tunit, new_points) do
    update_in(game, [key(:units), tid], fn tu -> %{tu | points: new_points} end)
  end

  defp move_unit(unit, in_range, game) do
    pos = unit.position

    occupied = MapSet.new(for {_, u} <- game.units, do: u.position)

    distances =
      calculate_distances([{pos, 0}], %{pos => {0, :self}}, MapSet.new(), occupied, game.map)

    best_targets =
      for {pos, {dist, _parent}} <- distances, MapSet.member?(in_range, pos), do: {dist, pos}

    case Enum.sort(best_targets) do
      [] ->
        unit

      [{_dist, pos} | _rest] ->
        new_pos = find_best_pos(distances, pos)
        %{unit | position: new_pos}
    end
  end

  defp find_best_pos(distances, pos) do
    case distances[pos] do
      {d, parent} when d > 1 -> find_best_pos(distances, parent)
      {_, _parent} -> pos
    end
  end

  defp calculate_distances([], distances, _, _, _), do: distances

  defp calculate_distances([{pos, dist} | rest], distances, seen, occupied, map) do
    {distances, to_visit} =
      Enum.reduce(cross(pos), {distances, rest}, fn tpos, {distances, to_visit} ->
        cond do
          # It's a wall!
          Map.has_key?(map, tpos) ->
            {distances, to_visit}

          # It's another unit
          MapSet.member?(occupied, tpos) ->
            {distances, to_visit}

          true ->
            new_dist = dist + 1

            # If not distance calculated or new distance is less, add it!
            distances =
              if not Map.has_key?(distances, tpos) or distances[tpos] > {new_dist, pos} do
                # Add distance + parent, to backtrack the best path
                Map.put(distances, tpos, {new_dist, pos})
              else
                distances
              end

            # If not visited and not pending to visit, let's visit it after!
            to_visit =
              if not MapSet.member?(seen, tpos) and
                   Enum.all?(to_visit, fn {p, _d} -> p != tpos end) do
                to_visit ++ [{tpos, new_dist}]
              else
                to_visit
              end

            {distances, to_visit}
        end
      end)

    seen = MapSet.put(seen, pos)

    calculate_distances(to_visit, distances, seen, occupied, map)
  end

  defp range_units(id, targets, game) do
    occupied = MapSet.new(for {tid, tunit} <- game.units, tid != id, do: tunit.position)

    all_pos =
      Enum.flat_map(targets, fn {_id, unit} ->
        unit.position
        |> cross
        |> Enum.filter(fn pos ->
          # Check walls and remove already occupied positions
          not Map.has_key?(game.map, pos) and not MapSet.member?(occupied, pos)
        end)
      end)

    MapSet.new(all_pos)
  end

  defp cross({x, y}) do
    [{x + 1, y}, {x, y + 1}, {x - 1, y}, {x, y - 1}]
  end

  defp parse_game(input) do
    input
    |> String.split("\n", strip: true)
    |> Stream.with_index()
    |> Enum.reduce(%Game{}, fn line, game ->
      parse_line(line, game)
    end)
  end

  defp parse_line({line, x}, game) do
    line
    |> String.codepoints()
    |> Stream.with_index()
    |> Enum.reduce(game, fn {char, y}, game ->
      parse_char(char, {x, y}, game)
    end)
  end

  # Only count walls on the map!
  defp parse_char("#", point, game), do: add_map(game, point, "#")
  defp parse_char(".", _point, game), do: game

  defp parse_char(u, point, game) when u in ["G", "E"] do
    {game, _id} = add_unit(game, point, u)
    game
    # add_map(game, point, {:unit, id})
  end

  defp add_map(game, point, u) do
    update_in(game, [key(:map), point], fn _ -> u end)
  end

  defp add_unit(game, point, u) do
    unit = %Unit{kind: u, position: point}
    id = generate_id()
    game = update_in(game, [key(:units), id], fn _ -> unit end)
    {game, id}
  end

  @chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> String.split("") |> Enum.filter(&(&1 != ""))
  defp generate_id() do
    1..10
    |> Enum.reduce([], fn _i, acc ->
      [Enum.random(@chars) | acc]
    end)
    |> Enum.join("")
  end

  # BEAUTY PRINT GAME!

  defp print_game(%{map: map, units: units, round: round}) do
    IO.puts("\n\nROUND: #{round}")
    {mx, my} = Enum.max(Map.keys(map))

    Enum.each(Range.new(0, mx), fn x ->
      Enum.map(Range.new(0, my), fn y ->
        p = {x, y}

        u = Enum.find(units, fn {_u, unit} -> unit.position == p end)

        cond do
          not is_nil(map[p]) -> map[p]
          not is_nil(u) -> elem(u, 1).kind
          true -> "."
        end
      end)
      |> Enum.join("")
      |> IO.puts()
    end)
  end
end
