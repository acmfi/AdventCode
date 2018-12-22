defmodule Solve do
  use Bitwise, only_operators: true

  def ex(path \\ "input_test") do
    input = path |> File.read!() |> parse()

    ex1 = execute(input, [0, 0, 0, 0, 0, 0])
    IO.puts("Star 1: #{ex1}")

    ex2 = execute(input, [1, 0, 0, 0, 0, 0])
    IO.puts("Star 2: #{ex2}")
  end

  defp execute({code, ip}, regs) do
    instr_p = Enum.at(regs, ip)

    execute_op(Map.get(code, instr_p), regs, {code, ip}, instr_p)
  end

  defp execute_op(nil, [r0 | _], _, _), do: r0

  # When next_instr = 1 -> calculate sum of factors of r2 (specific for every user)
  defp execute_op(_, [_r0, _r1, r2, _r3, _r4, _r5], _, 1) do
    Range.new(1, r2 + 1) |> Stream.filter(fn x -> rem(r2, x) == 0 end) |> Enum.sum()
  end

  defp execute_op({opcode, a, b, c}, regs, {code, ip}, _) do
    new_regs = op(opcode, regs, a, b, c)
    next_instr = Enum.at(new_regs, ip) + 1
    regs = List.replace_at(new_regs, ip, next_instr)
    execute({code, ip}, regs)
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

  defp parse(t) do
    [ip | code] = t |> String.split("\n")

    ip = parse_ip(ip)

    code =
      code
      |> Stream.map(&parse_code/1)
      |> Stream.with_index()
      |> Stream.map(fn {x, i} -> {i, x} end)
      |> Enum.into(%{})

    {code, ip}
  end

  defp parse_ip(t) do
    [_, ip] = String.split(t, " ")
    to_int(ip)
  end

  defp parse_code(l) do
    [op, a, b, c] = String.split(l, " ")
    op = String.to_atom(op)
    {op, to_int(a), to_int(b), to_int(c)}
  end

  defp to_int(x), do: String.to_integer(x)
end
