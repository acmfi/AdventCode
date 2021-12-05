defmodule Aoc.Aoc2021.Day04.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1({numbers, boards}) do
    {%{board: winning_board}, winning_number} =
      numbers
      |> Enum.reduce_while(boards, fn number, boards ->
        boards = mark_number(boards, number)

        case find_winner(boards) do
          nil -> {:cont, boards}
          board -> {:halt, {board, number}}
        end
      end)

    unmarked =
      Enum.reject(winning_board, fn {_index, {_n, marked}} -> marked end)
      |> Enum.map(fn {_index, {n, _marked}} -> n end)

    Enum.sum(unmarked) * winning_number
  end

  def star2({numbers, boards}) do
    {_boards_left, {%{board: winning_board}, winning_number}} =
      numbers
      |> Enum.reduce({boards, nil}, fn
        number, {boards, last_winner} ->
          boards = mark_number(boards, number)

          {winners, left} = Enum.split_with(boards, &board_winner?/1)

          case winners do
            [] -> {left, last_winner}
            _ -> {left, {Enum.at(winners, -1), number}}
          end
      end)

    unmarked =
      Enum.reject(winning_board, fn {_index, {_n, marked}} -> marked end)
      |> Enum.map(fn {_index, {n, _marked}} -> n end)

    Enum.sum(unmarked) * winning_number
  end

  defp mark_number(boards, number) do
    Enum.map(boards, fn %{board: board, indexes: indexes} ->
      board =
        case Map.fetch(indexes, number) do
          {:ok, index} ->
            Map.update!(board, index, fn {number, _} -> {number, true} end)

          _ ->
            board
        end

      %{board: board, indexes: indexes}
    end)
  end

  defp find_winner(boards) do
    Enum.find(boards, &board_winner?/1)
  end

  defp board_winner?(%{board: board}) do
    0..4
    |> Enum.reduce_while(false, fn index, _ ->
      cond do
        all_rows?(board, index) -> {:halt, true}
        all_columns?(board, index) -> {:halt, true}
        true -> {:cont, false}
      end
    end)
  end

  defp all_rows?(board, column) do
    0..4
    |> Enum.all?(fn
      row ->
        i = {row, column}
        {_n, marked} = Map.get(board, i)
        marked
    end)
  end

  defp all_columns?(board, row) do
    0..4
    |> Enum.all?(fn
      column ->
        i = {row, column}
        {_n, marked} = Map.get(board, i)
        marked
    end)
  end

  defp parse_board(board) do
    board =
      String.split(board, "\n", trim: true)
      |> Enum.with_index()
      |> Enum.flat_map(fn {row, rowi} ->
        String.split(row, " ", trim: true)
        |> Enum.map(&String.to_integer/1)
        |> Enum.with_index()
        |> Enum.map(fn {n, columni} ->
          {{rowi, columni}, {n, false}}
        end)
      end)
      |> Map.new()

    indexes = Map.new(board, fn {index, {number, _}} -> {number, index} end)
    %{board: board, indexes: indexes}
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
    [numbers | boards] = String.split(t, "\n\n", trim: true)

    numbers = String.split(numbers, ",", trim: true) |> Enum.map(&String.to_integer/1)
    boards = boards |> Enum.map(&parse_board/1)

    {numbers, boards}
  end
end
