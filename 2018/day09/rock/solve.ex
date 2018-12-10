defmodule Solve do
  def ex(path \\ "input_test") do
    path
    |> File.read!()
    |> String.split("\n")
    |> Enum.map(fn l ->
      l |> parse_input() |> play()
    end)
  end

  @factor 100
  defp play({players, last_marble}) do
    last_marble = last_marble * @factor
    IO.puts("Playing for #{players} players until #{last_marble} marble")
    start = new_board()

    winners_points = turns(start, 1, players, last_marble, %{})

    winners_points
    |> Enum.sort_by(&elem(&1, 1), &>=/2)
    |> Enum.at(0)
    |> elem(1)
    |> IO.puts()
  end

  @winner_factor 23
  defguard make_points(x) when rem(x, @winner_factor) == 0

  defp turns(_turn, marble, _players, last_marble, points) when marble > last_marble, do: points

  @winner_shift 7
  defp turns(board, marble, players, last_marble, points)
       when make_points(marble) do
    board = rotate_ln(board, @winner_shift)
    removed = current_board(board)
    board = remove_current(board)

    points_acquired = marble + removed
    winner_player = rem(marble, players)
    last_points = points[winner_player] || 0
    points = Map.put(points, winner_player, last_points + points_acquired)

    turns(board, marble + 1, players, last_marble, points)
  end

  defp turns(board, marble, players, last_marble, points) do
    board = board |> rotate_rn(2) |> add_current(marble)

    turns(board, marble + 1, players, last_marble, points)
  end

  defp new_board(), do: {[], 0, []}

  defp current_board({_, c, _}), do: c

  defp rotate_r({left, current, []}) do
    [new_current | right] = Enum.reverse([current | left])
    {[], new_current, right}
  end

  defp rotate_r({left, current, [new_current | right]}) do
    {[current | left], new_current, right}
  end

  defp rotate_rn(board, 0), do: board
  defp rotate_rn(board, n), do: board |> rotate_r() |> rotate_rn(n - 1)

  defp rotate_l({[], current, right}) do
    {r, c, l} = rotate_r({right, current, []})
    {l, c, r}
  end

  defp rotate_l({[new_current | left], current, right}) do
    {left, new_current, [current | right]}
  end

  defp rotate_ln(board, 0), do: board
  defp rotate_ln(board, n), do: board |> rotate_l() |> rotate_ln(n - 1)

  defp add_current({left, current, right}, new_current) do
    {left, new_current, [current | right]}
  end

  defp remove_current({left, _current, [current | right]}) do
    {left, current, right}
  end

  defp parse_input(text) do
    [players, _, _, _, _, _, last | _] = text |> String.split(" ", trim: true)
    {to_int(players), to_int(last)}
  end

  defp to_int(x), do: Integer.parse(x) |> elem(0)
end
