defmodule Solve do
  def ex(path \\ "input_test") do
    path |> File.read!() |> read_map() |> play()
  end

  defp play(state), do: play(state, 1)

  defp play(%{map: map, carts: carts, crashed: crashed}, tick) do
    {carts, new_crashed} =
      carts
      |> Enum.sort()
      |> tick(map, %{}, [])

    if length(crashed) == 0 and length(new_crashed) >= 1 do
      [{x, y}] = Enum.take(new_crashed, 1)
      IO.puts("First crash in tick #{tick} at: #{x},#{y}")
    end

    crashed = crashed ++ new_crashed

    case Enum.count(carts) == 1 do
      true ->
        [{{x, y}, _}] = carts |> Enum.to_list()
        IO.puts("Last cart standing at tick #{tick}: #{x},#{y}")

      _ ->
        play(%{map: map, carts: carts, crashed: crashed}, tick + 1)
    end
  end

  defp tick([], _map, new_carts, crashed), do: {new_carts, crashed}

  defp tick([{pos, cart} | rest], map, new_carts, crashed) do
    {new_carts, crashed} =
      case pos in crashed do
        false ->
          {new_pos, new_cart} = move_cart(pos, cart, map)

          case crash?(pos, new_pos, rest, new_carts) do
            true ->
              {new_carts, crashed ++ [new_pos]}

            _ ->
              {Map.put(new_carts, new_pos, new_cart), crashed}
          end

        true ->
          {new_carts, crashed}
      end

    tick(rest, map, new_carts, crashed)
  end

  defp crash?(pos, new_pos, carts, new_carts) do
    carts = Enum.into(carts, %{})
    cs = carts |> Map.delete(pos) |> Map.merge(new_carts)
    Map.has_key?(cs, new_pos)
  end

  defp move_cart(pos, %{move: m, intersec: i}, map) do
    next_pos = calculate_pos(pos, m)

    next_hex = map[next_pos]

    new_m = calculate_move(m, next_hex, i)
    new_i = calculate_i(i, next_hex)

    {next_pos, %{move: new_m, intersec: new_i}}
  end

  defp calculate_pos({i, j}, :l), do: {i - 1, j}
  defp calculate_pos({i, j}, :r), do: {i + 1, j}
  defp calculate_pos({i, j}, :u), do: {i, j - 1}
  defp calculate_pos({i, j}, :d), do: {i, j + 1}

  # Intersections "+"

  # Front is the same
  defp calculate_move(i, :intersec, :f), do: i

  defp calculate_move(:l, :intersec, :l), do: :d
  defp calculate_move(:r, :intersec, :l), do: :u
  defp calculate_move(:u, :intersec, :l), do: :l
  defp calculate_move(:d, :intersec, :l), do: :r

  defp calculate_move(:l, :intersec, :r), do: :u
  defp calculate_move(:r, :intersec, :r), do: :d
  defp calculate_move(:u, :intersec, :r), do: :r
  defp calculate_move(:d, :intersec, :r), do: :l

  # Normal turns "/"
  defp calculate_move(:l, :tr, _), do: :d
  defp calculate_move(:r, :tr, _), do: :u
  defp calculate_move(:u, :tr, _), do: :r
  defp calculate_move(:d, :tr, _), do: :l

  # Normal turns "\"
  defp calculate_move(:l, :tl, _), do: :u
  defp calculate_move(:r, :tl, _), do: :d
  defp calculate_move(:u, :tl, _), do: :l
  defp calculate_move(:d, :tl, _), do: :r

  defp calculate_move(i, :conth, _), do: i
  defp calculate_move(i, :contv, _), do: i

  defp calculate_i(:l, :intersec), do: :f
  defp calculate_i(:f, :intersec), do: :r
  defp calculate_i(:r, :intersec), do: :l
  defp calculate_i(i, _), do: i

  defp read_map(input) do
    base = %{map: %{}, carts: %{}, crashed: []}

    input
    |> String.split("\n")
    |> Stream.with_index()
    |> Enum.reduce(base, fn {row, i}, acc ->
      row
      |> String.split("")
      # Split("") injects empty on first and last u.u
      |> Stream.drop(1)
      |> Stream.with_index()
      |> Enum.reduce(acc, fn {elem, j}, acc ->
        pos = {j, i}

        case elem do
          "/" ->
            add_map(acc, pos, :tr)

          "\\" ->
            add_map(acc, pos, :tl)

          "+" ->
            add_map(acc, pos, :intersec)

          "-" ->
            add_map(acc, pos, :conth)

          "|" ->
            add_map(acc, pos, :contv)

          c when c in [">", "<", "v", "^"] ->
            p = if c in [">", "<"], do: :conth, else: :contv

            acc |> add_map(pos, p) |> add_cart(pos, parse_orient(c))

          n when n in ["", " "] ->
            acc
        end
      end)
    end)
  end

  defp add_map(%{map: map} = state, pos, x), do: %{state | map: Map.put(map, pos, x)}

  defp add_cart(%{carts: carts} = state, pos, x) do
    cart = %{move: x, intersec: :l}
    %{state | carts: Map.put(carts, pos, cart)}
  end

  defp parse_orient(">"), do: :r
  defp parse_orient("<"), do: :l
  defp parse_orient("v"), do: :d
  defp parse_orient("^"), do: :u

  # defp print_map(map, carts) do
  #   {max_x, max_y} =
  #     map
  #     |> Map.keys()
  #     |> Enum.reduce({0, 0}, fn {i, j}, {il, jl} -> {max(i, il), max(j, jl)} end)

  #   Enum.each(Range.new(0, max_y), fn y ->
  #     pos =
  #       Enum.map(Range.new(0, max_x), fn x ->
  #         pos = {x, y}

  #         case map[pos] do
  #           nil ->
  #             " "

  #           other ->
  #             case carts[pos] do
  #               nil -> pos_to_s(other)
  #               cart -> orient_to_s(cart.move)
  #             end
  #         end
  #       end)

  #     IO.puts(Enum.join(pos, ""))
  #   end)
  # end

  # defp pos_to_s(:tr), do: "/"
  # defp pos_to_s(:tl), do: "/"
  # defp pos_to_s(:intersec), do: "+"
  # defp pos_to_s(:conth), do: "-"
  # defp pos_to_s(:contv), do: "|"

  # defp orient_to_s(:l), do: "<"
  # defp orient_to_s(:r), do: ">"
  # defp orient_to_s(:u), do: "^"
  # defp orient_to_s(:d), do: "v"
end
