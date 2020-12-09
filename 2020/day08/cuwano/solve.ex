defmodule AdventCode.Aoc2020.Day08.Solve do
  @base_path Path.dirname(__ENV__.file)

  require Logger

  def execute(%{command: "nop"}, pc, counter), do: %{next_pc: pc + 1, next_counter: counter}

  def execute(%{command: "jmp", arg: arg}, pc, counter),
    do: %{next_pc: pc + arg, next_counter: counter}

  def execute(%{command: "acc", arg: arg}, pc, counter),
    do: %{next_pc: pc + 1, next_counter: counter + arg}

  def execute_program(command, input, pc, counter, executed_lines) do
    is_already_executed = pc in executed_lines

    if is_already_executed do
      counter
    else
      %{next_pc: next_pc, next_counter: next_counter} = execute(command, pc, counter)

      input
      |> Enum.at(next_pc)
      |> execute_program(input, next_pc, next_counter, [pc | executed_lines])
    end
  end

  def star1(input) do
    pc = 0
    counter = 0
    executed_lines = []

    input |> Enum.at(pc) |> execute_program(input, pc, counter, executed_lines)
  end

  def star2(input) do
    "TODO"
  end

  defp parse_line(line) do
    [command, arg] = line |> String.split(" ", trim: true)

    %{
      command: command,
      arg: String.to_integer(arg)
    }
  end

  # Helpers

  def solve(fname \\ "input") do
    do_solve(fname)
  end

  def solve_test(), do: solve("input_test")

  defp do_solve(fname) do
    path = Path.join(@base_path, fname)

    input = path |> File.read!() |> parse!()
    star1 = star1(input)
    IO.puts("Star 1: " <> to_string(star1))

    star2 = star2(input)
    IO.puts("Star 2: " <> to_string(star2))
  end

  defp parse!(t) do
    t
    |> String.split("\n")
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&parse_line/1)
  end
end
