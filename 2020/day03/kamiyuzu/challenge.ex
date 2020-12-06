defmodule AdventOfCode.Year2020.Day3.Challenge do
  @moduledoc """
  Impl day 3 of year 2020 advent of code
  """

  @path "test/fixtures/2020/day3/"

  @spec traverse_local_geology(String.t(), integer(), integer()) :: integer()
  def traverse_local_geology(input, right_steps, down_steps) do
    input
    |> parse_input()
    |> do_traverse_local_geology(right_steps, down_steps)
  end

  defp do_traverse_local_geology(parsed_input, right_steps, down_steps, tree_counter \\ 0)
  defp do_traverse_local_geology([], _, _, tree_counter), do: tree_counter

  defp do_traverse_local_geology(parsed_input, right_steps, down_steps, tree_counter) do
    right_walked_local_geology =
      Enum.reduce(parsed_input, [], fn local_geology_entry, acc ->
        {right_steps_taken, left_steps_to_iterate} = Enum.split(local_geology_entry, right_steps)
        acc ++ [left_steps_to_iterate ++ right_steps_taken]
      end)

    down_walked_local_geology = iterate_down_steps(right_walked_local_geology, down_steps)
    tree_to_add = check_topology(down_walked_local_geology)

    do_traverse_local_geology(
      down_walked_local_geology,
      right_steps,
      down_steps,
      tree_counter + tree_to_add
    )
  end

  defp iterate_down_steps(list, 0), do: list
  defp iterate_down_steps([_head | tail], down_steps), do:
    iterate_down_steps(tail, down_steps - 1)
  defp iterate_down_steps(list, _), do: list

  defp check_topology([]), do: 0

  defp check_topology(walked_topology) do
    [[head | _] | _] = walked_topology
    count_tree_elem(head)
  end

  defp count_tree_elem("#"), do: 1
  defp count_tree_elem(_), do: 0

  @spec traverse_local_geology_multiple_slopes(String.t(), [tuple()]) :: integer()
  def traverse_local_geology_multiple_slopes(input, steps) do
    input
    |> parse_input()
    |> do_traverse_local_geology_multiple_slopes(steps)
  end

  defp do_traverse_local_geology_multiple_slopes(parsed_input, steps) do
    steps
    |> Enum.map(fn {right_steps, down_steps} ->
      do_traverse_local_geology(parsed_input, right_steps, down_steps)
    end)
    |> Enum.reduce(1, fn tree_number, acc ->
      acc * tree_number
    end)
  end

  defp parse_input(input) do
    (@path <> input)
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes(&1))
  end
end
