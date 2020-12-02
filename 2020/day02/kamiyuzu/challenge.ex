defmodule AdventOfCode.Year2020.Day2.Challenge do
  @moduledoc """
  Impl day 1 of year 2020 advent of code
  """

  @path "test/fixtures/2020/day2/"

  @spec count_valid_passwords(String.t()) :: any()
  def count_valid_passwords(input) do
    input
    |> parse_input()
    |> do_count_valid_passwords()
  end

  defp do_count_valid_passwords(password_policy_list) do
    Enum.reduce(password_policy_list, 0, fn password_entry, password_counter ->
      {min, max, character, password} = parse_password_entry(password_entry)
      set_password_counter(min, max, character, password, password_counter)
    end)
  end

  defp parse_password_entry(password_entry) do
    [password_policy, password] = password_entry
    [min, max, character] = String.split(password_policy, ["-", " "])
    {min, max, character, password}
  end

  defp set_password_counter(min, max, character, password, password_counter) do
    min = String.to_integer(min)
    max = String.to_integer(max)

    character_list =
      password
      |> String.graphemes()
      |> Enum.filter(fn character_entry -> character_entry == character end)

    if length(character_list) >= min and length(character_list) <= max do
      password_counter + 1
    else
      password_counter
    end
  end

  @spec count_valid_passwords_second_part(String.t()) :: any()
  def count_valid_passwords_second_part(input) do
    input
    |> parse_input()
    |> do_count_valid_passwords_second_part()
  end

  defp do_count_valid_passwords_second_part(password_policy_list) do
    Enum.reduce(password_policy_list, 0, fn password_entry, password_counter ->
      {min, max, character, password} = parse_password_entry(password_entry)
      set_password_counter_second_part(min, max, character, password, password_counter)
    end)
  end

  defp set_password_counter_second_part(min, max, character, password, password_counter) do
    character_list = String.graphemes(password)

    if Enum.member?(character_list, character) do
      min_index = String.to_integer(min) - 1
      max_index = String.to_integer(max) - 1
      min_character = Enum.at(character_list, min_index)
      max_character = Enum.at(character_list, max_index)

      left_xor = min_character == character and not (max_character == character)
      right_xor = not (min_character == character) and max_character == character
      xor = left_xor or right_xor

      if xor do
        password_counter + 1
      else
        password_counter
      end
    else
      password_counter
    end
  end

  defp parse_input(input) do
    (@path <> input)
    |> File.read!()
    |> String.split("\n")
    |> Enum.map(&String.split(&1, ": "))
  end
end
