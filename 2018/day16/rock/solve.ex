defmodule Solve do
  use Bitwise, only_operators: true

  def ex(path \\ "input_test") do
    input = path |> File.read!() |> parse()

    s1 = ex1(input)
    IO.puts("STAR1: #{s1}")

    [s2 | _] = ex2(input)
    IO.puts("STAR2: #{s2}")
  end

  defp ex1({tests, _code}) do
    tests
    |> Stream.map(&find_possible_operations/1)
    |> Stream.filter(fn {_, codes} -> MapSet.size(codes) >= 3 end)
    |> Enum.count()
  end

  defp ex2({tests, code}) do
    opcodes = find_opcodes(tests)

    execute_program(code, opcodes)
  end

  # OPCODES

  @opcodes [
    :addr,
    :addi,
    :mulr,
    :muli,
    :banr,
    :bani,
    :borr,
    :bori,
    :setr,
    :seti,
    :gtir,
    :gtri,
    :gtrr,
    :eqir,
    :eqri,
    :eqrr
  ]

  @ops_s Enum.count(@opcodes)

  defp execute_program(code, opcodes), do: execute_program(code, [0, 0, 0, 0], opcodes)
  defp execute_program([], regs, _), do: regs

  defp execute_program([{op, a, b, c} | code], regs, opcodes) do
    opcode = opcodes[op]
    new_regs = op(opcode, regs, a, b, c)
    execute_program(code, new_regs, opcodes)
  end

  defp find_opcodes(tests) do
    tests
    |> Stream.map(&find_possible_operations/1)
    |> Enum.reduce(%{}, fn {op, codes}, result ->
      Map.update(result, op, codes, fn old -> MapSet.intersection(old, codes) end)
    end)
    |> reduce_opcodes(%{})
  end

  defp reduce_opcodes(operations, unique) do
    case Enum.count(unique) == @ops_s do
      true ->
        unique

      _ ->
        unique = operations |> add_reduced(unique)
        operations = remove_uniques(operations, unique)

        reduce_opcodes(operations, unique)
    end
  end

  defp add_reduced(operations, unique) do
    Enum.reduce(operations, unique, fn {c, ops}, unique ->
      case MapSet.size(ops) == 1 do
        true ->
          [op] = MapSet.to_list(ops)
          Map.put(unique, c, op)

        _ ->
          unique
      end
    end)
  end

  defp remove_uniques(operations, unique) do
    Enum.reduce(unique, operations, fn {_, op}, operations ->
      Enum.map(operations, fn {id, ops} ->
        {id, MapSet.delete(ops, op)}
      end)
      |> Enum.into(%{})
    end)
  end

  defp find_possible_operations({br, codes, ba}) do
    {op, a, b, c} = codes

    codes =
      Enum.reduce(@opcodes, MapSet.new(), fn opcode, total ->
        set = op(opcode, br, a, b, c)

        if eq_registers(set, ba) do
          MapSet.put(total, opcode)
        else
          total
        end
      end)

    {op, codes}
  end

  # Addition
  defp op(:addr, rs, a, b, c) do
    cv = Enum.at(rs, a) + Enum.at(rs, b)
    set_r(rs, c, cv)
  end

  defp op(:addi, rs, a, b, c) do
    cv = Enum.at(rs, a) + b
    set_r(rs, c, cv)
  end

  # Multiplication
  defp op(:mulr, rs, a, b, c) do
    cv = Enum.at(rs, a) * Enum.at(rs, b)
    set_r(rs, c, cv)
  end

  defp op(:muli, rs, a, b, c) do
    cv = Enum.at(rs, a) * b
    set_r(rs, c, cv)
  end

  # Bitwise AND
  defp op(:banr, rs, a, b, c) do
    cv = Enum.at(rs, a) &&& Enum.at(rs, b)
    set_r(rs, c, cv)
  end

  defp op(:bani, rs, a, b, c) do
    cv = Enum.at(rs, a) &&& b
    set_r(rs, c, cv)
  end

  # Bitwise OR
  defp op(:borr, rs, a, b, c) do
    cv = Enum.at(rs, a) ||| Enum.at(rs, b)
    set_r(rs, c, cv)
  end

  defp op(:bori, rs, a, b, c) do
    cv = Enum.at(rs, a) ||| b
    set_r(rs, c, cv)
  end

  # Assignment
  defp op(:setr, rs, a, _b, c) do
    cv = Enum.at(rs, a)
    set_r(rs, c, cv)
  end

  defp op(:seti, rs, a, _b, c) do
    set_r(rs, c, a)
  end

  # Greater-than
  defp op(:gtir, rs, a, b, c) do
    cv = gt(a, Enum.at(rs, b))
    set_r(rs, c, cv)
  end

  defp op(:gtri, rs, a, b, c) do
    cv = gt(Enum.at(rs, a), b)
    set_r(rs, c, cv)
  end

  defp op(:gtrr, rs, a, b, c) do
    cv = gt(Enum.at(rs, a), Enum.at(rs, b))
    set_r(rs, c, cv)
  end

  # Equality
  defp op(:eqir, rs, a, b, c) do
    cv = eq(a, Enum.at(rs, b))
    set_r(rs, c, cv)
  end

  defp op(:eqri, rs, a, b, c) do
    cv = eq(Enum.at(rs, a), b)
    set_r(rs, c, cv)
  end

  defp op(:eqrr, rs, a, b, c) do
    cv = eq(Enum.at(rs, a), Enum.at(rs, b))
    set_r(rs, c, cv)
  end

  defp set_r(rs, r, v), do: List.replace_at(rs, r, v)
  defp gt(b, c), do: if(b > c, do: 1, else: 0)
  defp eq(b, c), do: if(b == c, do: 1, else: 0)

  defp eq_registers([r0, r1, r2, r3], [r0, r1, r2, r3]), do: true
  defp eq_registers(_, _), do: false

  defp parse(t) do
    case t |> String.split("\n\n\n\n") do
      [tests] -> {parse_tests(tests), []}
      [tests, code] -> {parse_tests(tests), parse_code(code)}
    end
  end

  defp parse_tests(text) do
    text
    |> String.split("\n\n")
    |> Enum.map(fn t ->
      ["Before: " <> b, op, "After:  " <> a] = String.split(t, "\n")

      {parse_register(b), parse_op(op), parse_register(a)}
    end)
  end

  defp parse_code(code) do
    code |> String.split("\n") |> Enum.map(&parse_op/1)
  end

  defp parse_op(l) do
    [opcode, ra, rb, rc] = String.split(l, " ") |> Enum.map(&to_int/1)
    {opcode, ra, rb, rc}
  end

  defp parse_register(reg) do
    [r0, r1, r2, r3] = String.split(reg, ", ")
    r0 = String.trim(r0, "[")
    r3 = String.trim(r3, "]")

    [r0, r1, r2, r3] |> Enum.map(&to_int/1)
  end

  defp to_int(x), do: String.to_integer(x)
end
