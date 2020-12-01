defmodule AdventOfCode.Year2020.Day1 do
  @moduledoc """
  Impl day 1 of year 2020 advent of code
  """

  @path "test/fixtures/2020/day1/"

  @spec get_pair_summing_given_number(String.t(), integer()) :: tuple()
  def get_pair_summing_given_number(input, sum) do
    input
    |> parse_input()
    |> traverse_for_pair(sum)
  end

  defp traverse_for_pair([_ | tail] = list, sum) do
    do_traverse_for_pair(list, tail, sum)
  end

  defp do_traverse_for_pair([first | _], [first_copy | _], sum) when sum - first == first_copy do
    {first, first_copy, first * first_copy}
  end
  defp do_traverse_for_pair([_first, second | tail], [], sum), do:
    do_traverse_for_pair([second | tail], tail, sum)
  defp do_traverse_for_pair([first | tail], [_ | copy_tail], sum) do
    do_traverse_for_pair([first | tail], copy_tail, sum)
  end
  defp do_traverse_for_pair([_], [], _), do: {:error, :sum_pair_not_found}


  @spec get_third_summing_given_number(String.t(), integer()) :: tuple()
  def get_third_summing_given_number(input, sum) do
    input
    |> parse_input()
    |> traverse_for_third(sum)
  end

  defp traverse_for_third([_, _], _), do: {:error, :sum_third_not_found}
  defp traverse_for_third([first | tail] = list, sum) do
    case traverse_for_pair(list, sum - first) do
      {second, third, _sum_pair} -> {first, second, third, first * second * third}
      {:error, :sum_pair_not_found} -> traverse_for_third(tail, sum)
    end
  end

  defp parse_input(input) do
    @path <> input
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer/1)
  end
end
