defmodule AdventOfCode.Year2020.Day8.Challenge do
  @moduledoc """
  Impl day 8 of year 2020 advent of code
  """

  @path "test/fixtures/2020/day8/"
  @ets_name :console_boot
  @counter :counter

  @spec boot_console(String.t()) :: integer()
  def boot_console(input) do
    input
    |> parse_input()
    |> do_boot_console()
  end

  defp do_boot_console(parsed_input) do
    :ets.new(@ets_name, [:duplicate_bag, :named_table])

    {_, boot_counter} =
      parsed_input
      |> parse_instructions()
      |> traverse_instructions()

    boot_counter
  end

  defp traverse_instructions(instructions, instr_index \\ 0, executed_instr \\ [])
  defp traverse_instructions([], instr_index, _), do: {instr_index, get_boot_arguments()}

  defp traverse_instructions(instructions, instr_index, executed_instr) do
    if instr_index in executed_instr or instr_index > length(instructions) do
      traverse_instructions([], instr_index, executed_instr)
    else
      instruction = Enum.at(instructions, instr_index)
      next_intrs_index = execute_operation(instruction)

      traverse_instructions(
        instructions,
        instr_index + next_intrs_index,
        executed_instr ++ [instr_index]
      )
    end
  end

  defp get_boot_arguments() do
    :ets.lookup(:console_boot, :counter)
    |> Enum.map(fn {:counter, amount} -> amount end)
    |> Enum.sum()
  end

  defp execute_operation(nil), do: 0
  defp execute_operation({"jmp", argument}), do: argument
  defp execute_operation({"nop", _argument}), do: 1

  defp execute_operation({"acc", argument}) do
    :ets.insert(@ets_name, {@counter, argument})
    1
  end

  defp parse_instructions(parsed_input, acc \\ [])
  defp parse_instructions([], acc), do: acc

  defp parse_instructions([instr | tail], acc) do
    [operation, argument] = String.split(instr, " ")
    parsed_instruction = {operation, String.to_integer(argument)}
    parse_instructions(tail, acc ++ [parsed_instruction])
  end

  @spec repair_boot_console(String.t()) :: integer()
  def repair_boot_console(input) do
    input
    |> parse_input()
    |> do_repair_boot_console()
  end

  defp do_repair_boot_console(parsed_input) do
    :ets.new(@ets_name, [:duplicate_bag, :named_table])

    parsed_input
    |> parse_instructions()
    |> repair_instruction()
  end

  defp repair_instruction(instructions, to_repair_operations \\ ["jmp", "nop"])

  defp repair_instruction(instructions, to_repair_operations) do
    [solution] =
      to_repair_operations
      |> Enum.map(fn operation -> get_swapable_indexes(instructions, operation) end)
      |> get_instruction_swapped_ops(instructions)
      |> get_bootable_arg_counter()

    solution
  end

  defp get_bootable_arg_counter(instruction_mapped_ops) do
    instruction_mapped_ops
    |> Enum.flat_map(fn operation_to_repair_instr_map ->
      Enum.map(operation_to_repair_instr_map, fn operation_to_repair_instr ->
        :ets.delete(@ets_name, @counter)
        {instr_index, arg_counter} = traverse_instructions(operation_to_repair_instr)

        if instr_index >= length(operation_to_repair_instr) do
          arg_counter
        end
      end)
    end)
    |> Enum.reject(fn x -> x == nil end)
  end

  defp get_instruction_swapped_ops(swappable_indexes, instructions) do
    Enum.map(swappable_indexes, fn operation_indexes ->
      Enum.map(operation_indexes, fn {operation, index} ->
        {_op, arg} = Enum.at(instructions, index)
        List.replace_at(instructions, index, {swap_instr(operation), arg})
      end)
    end)
  end

  defp swap_instr("jmp"), do: "nop"
  defp swap_instr("nop"), do: "jmp"

  defp get_swapable_indexes(instructions, repair_operation) do
    instructions
    |> Enum.with_index()
    |> Enum.map(fn {{operation, _argument}, index} ->
      if operation == repair_operation do
        {operation, index}
      end
    end)
    |> Enum.filter(fn repair_operation -> repair_operation != nil end)
  end

  defp parse_input(input) do
    (@path <> input)
    |> File.read!()
    |> String.split("\n")
  end
end
