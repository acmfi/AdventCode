defmodule AdventOfCode.Year2020.Day5.Challenge do
  @moduledoc """
  Impl day 5 of year 2020 advent of code
  """

  @path "test/fixtures/2020/day5/"

  @spec get_max_id_seat(String.t()) :: tuple()
  def get_max_id_seat(input) do
    input
    |> parse_input()
    |> do_get_max_id_seat()
  end

  defp decode_plane_seats(parsed_input), do: Enum.map(parsed_input, &decode_plane_seat/1)

  defp do_get_max_id_seat(parsed_input) do
    parsed_input
    |> decode_plane_seats()
    |> Enum.max_by(fn id -> id end)
  end

  defp decode_plane_seat(raw_plane_seat) do
    {parsed_row, raw_input} = raw_plane_seat |> String.graphemes() |> decode_plane_row()
    [_ | raw_seat] = raw_input
    parsed_seat = decode_plane_raw_seat(raw_seat)
    parsed_row * 8 + parsed_seat
  end

  def decode_plane_row(raw_entry_seat, range \\ {0, 127})

  def decode_plane_row(["F", _second, _third, _fourth] = raw_seat, {lower_limit, _upper_limit}),
    do: {lower_limit, raw_seat}

  def decode_plane_row(["B", _second, _third, _fourth] = raw_seat, {_lower_limit, upper_limit}),
    do: {upper_limit, raw_seat}

  def decode_plane_row(["F" | raw_entry_seat_tail], range) do
    parsed_row = get_new_upper_limit(range)
    decode_plane_row(raw_entry_seat_tail, parsed_row)
  end

  def decode_plane_row(["B" | raw_entry_seat_tail], range) do
    parsed_row = get_new_lower_limit(range)
    decode_plane_row(raw_entry_seat_tail, parsed_row)
  end

  def decode_plane_raw_seat(raw_entry_seat_tail, range \\ {0, 7})

  def decode_plane_raw_seat(["L"], range) do
    {lower_limit, _upper_limit} = get_new_upper_limit(range)
    lower_limit
  end

  def decode_plane_raw_seat(["R"], range) do
    {_lower_limit, upper_limit} = get_new_lower_limit(range)
    upper_limit
  end

  def decode_plane_raw_seat(["L" | raw_entry_seat_tail], range) do
    parsed_row = get_new_upper_limit(range)
    decode_plane_raw_seat(raw_entry_seat_tail, parsed_row)
  end

  def decode_plane_raw_seat(["R" | raw_entry_seat_tail], range) do
    parsed_row = get_new_lower_limit(range)
    decode_plane_raw_seat(raw_entry_seat_tail, parsed_row)
  end

  defp get_new_upper_limit({lower_limit, upper_limit}) do
    range = abs(upper_limit - lower_limit)
    upper_limit_decrement = div(range, 2)
    {lower_limit, lower_limit + upper_limit_decrement}
  end

  defp get_new_lower_limit({lower_limit, upper_limit}) do
    range = abs(lower_limit - upper_limit)
    lower_limit_decrement = div(range, 2) + 1
    {lower_limit + lower_limit_decrement, upper_limit}
  end

  @spec get_seat(String.t()) :: tuple()
  def get_seat(input) do
    input
    |> parse_input()
    |> do_get_seat()
  end

  defp do_get_seat(parsed_input) do
    range = 0..do_get_max_id_seat(parsed_input)
    parsed_seats = decode_plane_seats(parsed_input)

    [[_, seat_id]] =
      range
      |> Enum.filter(fn number -> Enum.member?(parsed_seats, number) == false end)
      |> Enum.chunk_every(2)
      |> Enum.filter(fn [x, y] -> x != y - 1 end)

    seat_id
  end

  defp parse_input(input) do
    (@path <> input)
    |> File.read!()
    |> String.split("\n")
  end
end
