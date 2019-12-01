defmodule Solve do
  use Bitwise, only_operators: true

  def ex(path \\ "input") do
    {code, ip} = path |> File.read!() |> parse()

    # Halt on instr 29
    code = code |> Map.put(29, {:seti, 1000, nil, ip})
    [_, s1 | _] = execute({code, ip}, [0, 0, 0, 0, 0, 0])
    IO.puts("Star 1: #{s1}")

    s2 = ex2({code, ip})
    IO.puts("Star 2: #{s2}")
  end

  defp ex2({code, ip}) do
    regs = [0, 0, 0, 0, 0, 0]

    # Replace loop with divi
    code =
      code
      |> Map.put(17, {:divi, 2, 256, 2})
      |> Map.put(18, {:seti, 7, nil, ip})

    find_target({code, ip}, regs, :no_targ, MapSet.new())
  end

  defp find_target({code, ip}, regs, last_target, targets) do
    # Start at 6 always, where the main logic is
    regs = set_r(regs, ip, 6)
    regs = execute({code, ip}, regs)
    target = Enum.at(regs, 1)

    case MapSet.member?(targets, target) do
      true ->
        last_target

      _ ->
        targets = MapSet.put(targets, target)
        find_target({code, ip}, regs, target, targets)
    end
  end

  defp execute({code, ip}, regs) do
    instr_p = Enum.at(regs, ip)
    instr = Map.get(code, instr_p)

    case execute_op(instr, regs, ip) do
      {:cont, regs} ->
        execute({code, ip}, regs)

      {:halt, regs} ->
        regs
    end
  end

  defp execute_op(nil, regs, _), do: {:halt, regs}

  defp execute_op({opcode, a, b, c}, regs, ip) do
    regs = op(opcode, regs, a, b, c)
    next_instr = Enum.at(regs, ip) + 1
    regs = set_r(regs, ip, next_instr)
    {:cont, regs}
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

  # Integer division to replace a loop
  defp op(:divi, rs, a, b, c) do
    cv = div(Enum.at(rs, a), b)
    set_r(rs, c, cv)
  end

  defp set_r([_r0 | rs], 0, v), do: [v | rs]
  defp set_r([r0, _r1 | rs], 1, v), do: [r0, v | rs]
  defp set_r([r0, r1, _r2 | rs], 2, v), do: [r0, r1, v | rs]
  defp set_r([r0, r1, r2, _r3 | rs], 3, v), do: [r0, r1, r2, v | rs]
  defp set_r([r0, r1, r2, r3, _r4 | rs], 4, v), do: [r0, r1, r2, r3, v | rs]
  defp set_r([r0, r1, r2, r3, r4, _r5 | rs], 5, v), do: [r0, r1, r2, r3, r4, v | rs]

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
