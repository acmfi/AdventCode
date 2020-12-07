defmodule AdventOfCode.Year2020.Day6.Challenge do
  @moduledoc """
  Impl day 6 of year 2020 advent of code
  """

  @path "test/fixtures/2020/day6/"

  @spec sum_group_answers(String.t()) :: any()
  def sum_group_answers(input) do
    input
    |> parse_input()
    |> do_count_group_answers()
  end

  defp do_count_group_answers(parsed_input) do
    parsed_input
    |> get_group_answers()
    |> Enum.map(&count_group_answers/1)
    |> Enum.sum()
  end

  defp get_group_answers(parsed_input) do
    parsed_input
    |> Enum.chunk_by(&(&1 == ""))
    |> Enum.filter(&(&1 != [""]))
  end

  defp count_group_answers(group_answers) do
    group_answers
    |> Enum.flat_map(&get_individual_answers/1)
    |> Enum.uniq()
    |> Enum.count(&is_binary(&1))
  end

  defp get_individual_answers(individual_answers), do: String.graphemes(individual_answers)

  @spec sum_group_everyone_answers(String.t()) :: any()
  def sum_group_everyone_answers(input) do
    input
    |> parse_input()
    |> do_sum_group_everyone_answers()
  end

  defp do_sum_group_everyone_answers(parsed_input) do
    parsed_input
    |> get_group_answers()
    |> Enum.map(&count_group_everyone_answers/1)
    |> Enum.sum()
  end

  defp count_group_everyone_answers(group_answers) do
    group_answers
    |> Enum.map(&get_individual_answers/1)
    |> count_unique_elem_in_all_group()
  end

  defp count_unique_elem_in_all_group([head | group_tail]) do
    Enum.count(head, &check_individual_response_with_group(&1, group_tail))
  end

  defp check_individual_response_with_group(response, group, response_checked \\ true)
  defp check_individual_response_with_group(_response, _group, false), do: false
  defp check_individual_response_with_group(_response, [], response_checked), do: response_checked
  defp check_individual_response_with_group(response, [head | group_tail], _is_present) do
    is_present = Enum.member?(head, response)
    check_individual_response_with_group(response, group_tail, is_present)
  end

  defp parse_input(input) do
    (@path <> input)
    |> File.read!()
    |> String.split("\n")
  end
end
