defmodule Ironjanowar do
  import NimbleParsec

  require Logger

  def solve1(path \\ "./input") do
    input =
      File.read!(path)
      |> String.split("\n", trim: true)
      |> Enum.map(&parse_command/1)
      |> Enum.sort()

    guard_id = input |> sum_hours_by_guard() |> get_map_max()

    hour = get_max_hour_by_id(guard_id, input)

    guard_id * hour
  end

  guard_command =
    ignore(string("Guard #"))
    |> integer(min: 1)
    |> ignore(string(" begins shift"))
    |> unwrap_and_tag(:shift)

  down_command = string("falls asleep") |> replace(:down)
  up_command = string("wakes up") |> replace(:up)

  defparsec(
    :parsec_command,
    ignore(string("["))
    |> integer(4)
    |> ignore(string("-"))
    |> integer(2)
    |> ignore(string("-"))
    |> integer(2)
    |> ignore(string(" "))
    |> integer(2)
    |> ignore(string(":"))
    |> integer(2)
    |> ignore(string("] "))
    |> choice([guard_command, down_command, up_command])
  )

  @doc """
  Parses a single command as a string

  ## Examples

    iex> Ironjanowar.parse_command("[1518-11-01 00:00] Guard #10 begins shift")
    {{1518, 11, 1}, 0, 0, {:shift, 10}}

    iex> Ironjanowar.parse_command("[1518-11-01 00:05] falls asleep")
    {{1518, 11, 1}, 0, 5, :down}

    iex> Ironjanowar.parse_command("[1518-11-01 00:25] wakes up")
    {{1518, 11, 1}, 0, 25, :up}

  """
  def parse_command(string) do
    {:ok, [year, month, day, hour, minute, command], _, _, _, _} = parsec_command(string)
    {{year, month, day}, hour, minute, command}
  end

  @doc """
  Meh

  iex> Ironjanowar.sum_hours_by_guard([
  ...> {{1518, 11, 1}, 0, 0, {:shift, 10}},
  ...> {{1518, 11, 1}, 0, 5, :down},
  ...> {{1518, 11, 1}, 0, 25, :up},
  ...> {{1518, 11, 1}, 0, 30, :down},
  ...> {{1518, 11, 1}, 0, 55, :up},
  ...> {{1518, 11, 1}, 23, 58, {:shift, 99}},
  ...> {{1518, 11, 2}, 0, 40, :down},
  ...> {{1518, 11, 2}, 0, 50, :up},
  ...> {{1518, 11, 3}, 0, 5, {:shift, 10}},
  ...> {{1518, 11, 3}, 0, 24, :down},
  ...> {{1518, 11, 3}, 0, 29, :up},
  ...> {{1518, 11, 4}, 0, 2, {:shift, 99}},
  ...> {{1518, 11, 4}, 0, 36, :down},
  ...> {{1518, 11, 4}, 0, 46, :up},
  ...> {{1518, 11, 5}, 0, 3, {:shift, 99}},
  ...> {{1518, 11, 5}, 0, 45, :down},
  ...> {{1518, 11, 5}, 0, 55, :up}
  ...> ])
  %{
    10 => 50,
    99 => 30
  }
  """
  def sum_hours_by_guard(commands) do
    sum_hours_by_guard(commands, %{})
  end

  defp sum_hours_by_guard([{_date, _hour, _minute, {:shift, id}} | rest], acc) do
    {hours, rest} = get_hours(rest)
    acc = Map.update(acc, id, hours, &(&1 + hours))
    sum_hours_by_guard(rest, acc)
  end

  defp sum_hours_by_guard([], acc), do: acc

  defp get_hours(commands) do
    get_hours(commands, 0)
  end

  defp get_hours([{_, _, down_time, :down}, {_, _, up_time, :up} | rest], hours) do
    new_hours = Enum.count(down_time..(up_time - 1))
    get_hours(rest, new_hours + hours)
  end

  defp get_hours(rest, hours) do
    {hours, rest}
  end

  @doc """
  Gets the key for the max value

  ## Examples

  iex> Ironjanowar.get_map_max(%{
  ...> 10 => 50,
  ...> 99 => 30
  ...> })
  10

  """
  def get_map_max(map) do
    {id, _} = Enum.max_by(map, fn {_id, hours} -> hours end)
    id
  end

  @doc """
  Gets the max hour of sleep of a guard

  ## Examples

  iex> Ironjanowar.get_max_hour_by_id(10, [
  ...> {{1518, 11, 1}, 0, 0, {:shift, 10}},
  ...> {{1518, 11, 1}, 0, 5, :down},
  ...> {{1518, 11, 1}, 0, 25, :up},
  ...> {{1518, 11, 1}, 0, 30, :down},
  ...> {{1518, 11, 1}, 0, 55, :up},
  ...> {{1518, 11, 1}, 23, 58, {:shift, 99}},
  ...> {{1518, 11, 2}, 0, 40, :down},
  ...> {{1518, 11, 2}, 0, 50, :up},
  ...> {{1518, 11, 3}, 0, 5, {:shift, 10}},
  ...> {{1518, 11, 3}, 0, 24, :down},
  ...> {{1518, 11, 3}, 0, 29, :up},
  ...> {{1518, 11, 4}, 0, 2, {:shift, 99}},
  ...> {{1518, 11, 4}, 0, 36, :down},
  ...> {{1518, 11, 4}, 0, 46, :up},
  ...> {{1518, 11, 5}, 0, 3, {:shift, 99}},
  ...> {{1518, 11, 5}, 0, 45, :down},
  ...> {{1518, 11, 5}, 0, 55, :up}
  ...> ])
  24

  """
  def get_max_hour_by_id(id, commands) do
    get_max_hour_by_id(id, commands, %{})
  end

  defp get_max_hour_by_id(id, [{_, _, _, {:shift, id}} | rest], hours) do
    {new_hours, rest} = get_all_hours(rest)

    hours = Enum.reduce(new_hours, hours, fn hour, acc -> Map.update(acc, hour, 1, &(&1 + 1)) end)

    get_max_hour_by_id(id, rest, hours)
  end

  defp get_max_hour_by_id(id, [{_, _, _, {:shift, _}} | rest], hours) do
    get_max_hour_by_id(id, rest, hours)
  end

  defp get_max_hour_by_id(_, [], hours), do: get_map_max(hours)

  defp get_max_hour_by_id(id, [_ | rest], hours),
    do: get_max_hour_by_id(id, rest, hours)

  defp get_all_hours(commands) do
    get_all_hours(commands, [])
  end

  defp get_all_hours([{_, _, down_time, :down}, {_, _, up_time, :up} | rest], hours) do
    new_hours = Enum.to_list(down_time..(up_time - 1)) ++ hours
    get_all_hours(rest, new_hours)
  end

  defp get_all_hours([{_, _, _, {:shift, _}} | rest], hours), do: {hours, rest}
end
