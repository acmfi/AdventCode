defmodule Cuwano do
  def test(star) do
    parse_and_solve("test_input", star)
  end

  def solve(star) do
    parse_and_solve("input", star)
  end

  defp parse_and_solve(file_name, star) do
    solve = if star in [:star1, 1], do: &star1/1, else: &star2/1

    file_name |> parse_input() |> solve.() |> IO.inspect()
  end

  defp parse_input(file) do
    file |> File.read!() |> parse()
  end

  def parse(text) do
    lines = String.split(text, "\n")
    stack_map = parse_stack_map(lines)
    instructions = parse_instructions(lines)

    %{stack_map: stack_map, instructions: instructions}
  end

  defp parse_stack_map(lines) do
    lines
    |> Enum.reduce_while(%{}, &reducer/2)
    |> Enum.map(fn {column, list} ->
      list = list |> Enum.reject(&is_nil/1) |> Enum.reverse()
      {column, list}
    end)
    |> Enum.into(%{})
  end

  defp reducer(line, acc) do
    if String.contains?(line, "1") do
      {:halt, acc}
    else
      acc = parse_stack_line(line, acc)
      {:cont, acc}
    end
  end

  defp parse_stack_line(line, acc) do
    line
    |> get_row()
    |> Enum.with_index(1)
    |> Enum.reduce(acc, fn {elem, index}, acc -> update_in(acc, [index], &[elem | &1 || []]) end)
  end

  defp get_row(line) when is_binary(line) do
    line |> String.graphemes() |> get_row([])
  end

  defp get_row(line, result) do
    case line do
      ["[", letter, "]", " " | rest] ->
        result = result ++ [letter]
        get_row(rest, result)

      [" ", " ", " ", " " | rest] ->
        result = result ++ [nil]
        get_row(rest, result)

      ["[", letter, "]"] ->
        result ++ [letter]

      [" ", " ", " "] ->
        result ++ [nil]
    end
  end

  defp parse_instructions(lines) do
    lines |> Enum.filter(&String.starts_with?(&1, "move")) |> Enum.map(&line_to_instruction/1)
  end

  defp line_to_instruction(line) do
    regex = ~r/move (?<amount>\d+) from (?<from>\d+) to (?<to>\d+)/
    %{"amount" => amount, "from" => from, "to" => to} = Regex.named_captures(regex, line)

    %{amount: String.to_integer(amount), from: String.to_integer(from), to: String.to_integer(to)}
  end

  def star1(%{stack_map: stack_map, instructions: instructions}) do
    instructions
    |> Enum.reduce(stack_map, &apply_instruction/2)
    |> Enum.sort_by(fn {index, _} -> index end)
    |> Enum.map(fn {_, [last | _]} -> last end)
    |> Enum.join()
  end

  defp apply_instruction(%{amount: amount, from: from, to: to}, stacks) do
    apply_instruction(amount, from, to, stacks)
  end

  defp apply_instruction(0, _, _, stacks), do: stacks

  defp apply_instruction(amount, from, to, stacks) do
    {elem, from_stack} = List.pop_at(stacks[from], 0)
    to_stack = [elem | stacks[to]]

    stacks =
      stacks
      |> Map.put(from, from_stack)
      |> Map.put(to, to_stack)

    apply_instruction(amount - 1, from, to, stacks)
  end

  def star2(%{stack_map: stack_map, instructions: instructions}) do
    instructions
    |> Enum.reduce(stack_map, &apply_instruction_2/2)
    |> Enum.sort_by(fn {index, _} -> index end)
    |> Enum.map(fn {_, [last | _]} -> last end)
    |> Enum.join()
  end

  defp apply_instruction_2(%{amount: amount, from: from, to: to}, stacks) do
    apply_instruction_2(amount, from, to, stacks)
  end

  defp apply_instruction_2(amount, from, to, stacks) do
    elems = Enum.take(stacks[from], amount)
    from_stack = Enum.slice(stacks[from], amount..-1)

    to_stack = elems ++ stacks[to]

    stacks
    |> Map.put(from, from_stack)
    |> Map.put(to, to_stack)
  end
end
