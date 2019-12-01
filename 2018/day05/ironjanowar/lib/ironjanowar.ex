defmodule Ironjanowar do
  require Logger

  @doc """
  Analyzes the string and returns the new string and the number of changes

  ## Examples

      iex> Ironjanowar.react("dabAcCaCBAcCcaDA")
      'ADacABCbad'

  """
  def react(string) when is_binary(string),
    do: string |> String.to_charlist() |> react

  def react(list), do: react(list, [])

  def react([f, s | rest], acc) do
    case {abs(f - s), acc} do
      {32, []} -> react(rest, acc)
      {32, [prev | acc_rest]} -> react([prev | rest], acc_rest)
      _ -> react([s | rest], [f | acc])
    end
  end

  def react([f], acc), do: [f | acc]

  @doc """
    Checks the length of the reaction

  ## Examples

      iex> Ironjanowar.react_length("dabAcCaCBAcCcaDA")
      10
  """
  def react_length(polymer), do: polymer |> react() |> length()

  @doc """
  Solves first star

  ## Examples

  iex> Ironjanowar.solve1("./test_input")
  10
  """
  def solve1(file \\ "./input") do
    file |> File.read!() |> react_length()
  end

  def get_letters_from_polymer(polymer) when is_binary(polymer) do
    polymer
    |> String.upcase()
    |> String.to_charlist()
    |> MapSet.new()
    |> MapSet.to_list()
  end

  def get_min_reaction(polymer) do
    letters = polymer |> get_letters_from_polymer()

    Enum.map(letters, fn l ->
      polymer
      |> String.to_charlist()
      |> Enum.filter(fn x -> x != l and x != l + 32 end)
      |> react_length
    end)
    |> Enum.min()
  end

  @doc """
  Solves the second star

  ## Examples

    iex> Ironjanowar.solve2("./test_input")
    4
  """
  def solve2(file \\ "./input") do
    file |> File.read!() |> get_min_reaction()
  end
end
