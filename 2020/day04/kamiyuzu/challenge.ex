defmodule AdventOfCode.Year2020.Day4.Challenge do
  @moduledoc """
  Impl day 4 of year 2020 advent of code
  """

  @path "test/fixtures/2020/day4/"

  defmodule Passport do
    @required_keys ~w(byr iyr eyr hgt hcl ecl pid)a
    @valid_keys @required_keys ++ ~w(cid)a

    @enforce_keys @required_keys
    defstruct @valid_keys

    @valid_eye_color ~w(amb blu brn gry grn hzl oth)
    @valid_hair_color_characters ~w(a b c d e f)

    @spec new(map()) :: %Passport{} | ArgumentError | :not_valid
    def new(fields) do
      errors = get_erroed_fields(fields)

      if length(errors) == 0 do
        struct!(Passport, fields)
      else
        :not_valid
      end
    end

    defp get_erroed_fields(fields), do: fields |> Enum.map(&validate_field/1) |> Enum.filter(&(&1 == :error))

    defp validate_field({:byr, value}), do: validate_year_field(value, 1920, 2002)
    defp validate_field({:iyr, value}), do: validate_year_field(value, 2010, 2020)
    defp validate_field({:eyr, value}), do: validate_year_field(value, 2020, 2030)
    defp validate_field({:hgt, <<inches::binary-size(2)>> <> "in"}), do: validate_height_field(inches, 59, 76)
    defp validate_field({:hgt, <<centimeters::binary-size(3)>> <> "cm"}), do: validate_height_field(centimeters, 150, 193)
    defp validate_field({:ecl, value}) when value in @valid_eye_color, do: :ok
    defp validate_field({:cid, _value}), do: :ok

    defp validate_field({:pid, value}) do
      if String.length(value) == 9 do
        :ok
      else
        :error
      end
    end

    defp validate_field({:hcl, "#" <> <<hair_color::binary-size(6)>>}) do
        valid_characters =
          hair_color
        |> String.graphemes()
        |> Enum.map(&validate_hair_color_character/1)
        |> Enum.filter(&(&1 == :ok))

        length(valid_characters) == 6
    end
    defp validate_field(_), do: :error

    defp validate_hair_color_character(hair_color_character)
         when hair_color_character in @valid_hair_color_characters,
         do: :ok

    defp validate_hair_color_character(hair_color_character) do
      parsed_character = String.to_integer(hair_color_character)

      if parsed_character >= 0 and parsed_character <= 9 do
        :ok
      else
        :error
      end
    end

    defp validate_year_field(value, start_year, ending_year) do
      parsed_value = String.to_integer(value)

      case parsed_value >= start_year and parsed_value <= ending_year and String.length(value) == 4 do
        false -> :error
        true -> :ok
      end
    end

    defp validate_height_field(height, start_height, ending_height) do
      case String.to_integer(height) >= start_height and String.to_integer(height) <= ending_height do
        false -> :error
        true -> :ok
      end
    end
  end

  @spec validate_transports(String.t(), atom()) :: integer()
  def validate_transports(input, opts) do
    input
    |> parse_input()
    |> do_validate_transports(opts)
  end

  defp do_validate_transports(parsed_passports_entries, opts) do
    parsed_passports_entries
    |> parse_entries(opts)
    |> Enum.reduce(0, fn passport, acc ->
      case passport do
        %Passport{} -> acc + 1
        :not_valid -> acc
      end
    end)
  end

  defp parse_entries(entries, opts) do
    Enum.map(entries, fn passport_entry ->
      passport_entry
      |> Enum.map(&(&1 |> parse_passport_entry_fields() |> set_passport_entry_fields()))
      |> merge_passport_entry_fields()
      |> validate_passport_struct(opts)
    end)
  end

  defp parse_passport_entry_fields(fields) do
    fields
    |> String.split(" ")
    |> Enum.map(&String.split(&1, ":"))
  end

  defp set_passport_entry_fields(fields_list) do
    Enum.reduce(fields_list, %{}, fn [key, value], acc ->
      Map.put(acc, String.to_existing_atom(key), value)
    end)
  end

  defp merge_passport_entry_fields(passport_fields_maps) do
    Enum.reduce(passport_fields_maps, %{}, fn passport_fields_map, acc ->
      Map.merge(acc, passport_fields_map)
    end)
  end

  defp validate_passport_struct(fields, :no_extra_validation) do
    try do
      struct!(Passport, fields)
    rescue
      ArgumentError -> :not_valid
    end
  end

  defp validate_passport_struct(fields, :extra_validation) do
    try do
      Passport.new(fields)
    rescue
      ArgumentError -> :not_valid
    end
  end

  defp parse_input(input) do
    (@path <> input)
    |> File.read!()
    |> String.split("\n\n")
    |> Enum.map(&String.split(&1, "\n"))
  end
end
