defmodule Aoc.Aoc2020.Day18.Solve do
  @base_path Path.dirname(__ENV__.file)

  def star1(input) do
    Enum.map(input, &exec_expr/1) |> Enum.sum()
  end

  @star2_priors %{
    eof: 1,
    expr: 2,
    add: 10,
    mul: 20
  }

  def star2(input) do
    Enum.map(input, &exec_expr(&1, @star2_priors)) |> Enum.sum()
  end

  @default_priors %{
    eof: 1,
    expr: 2,
    add: 3,
    mul: 3
  }

  defp exec_expr(expr, priors \\ @default_priors)
  defp exec_expr([r], _), do: r

  defp exec_expr([l, op, r | rest], priors) do
    cond do
      is_list(l) ->
        l_v = exec_expr(l)
        exec_expr([l_v, op, r | rest], priors)

      is_list(r) ->
        r_v = exec_expr(r, priors)
        exec_expr([l, op, r_v | rest], priors)

      op_pri(op, priors) > op_pri(next_op(rest), priors) ->
        r_v = exec_expr([r | rest], priors)
        op(l, op, r_v)

      true ->
        v = op(l, op, r)
        exec_expr([v | rest], priors)
    end
  end

  defp op_pri(op, priors), do: Map.get(priors, op, 99)

  defp next_op([]), do: :eof
  defp next_op([x | _]) when is_list(x), do: :expr
  defp next_op([op | _]), do: op

  defp op(l, :add, r), do: l + r
  defp op(l, :mul, r), do: l * r

  defp parse_line(line) do
    line |> String.split(" ", trim: true) |> parse_expr()
  end

  defp parse_expr(toks, expr \\ [])
  defp parse_expr([], expr), do: expr

  defp parse_expr([x | rest], expr) do
    case x do
      "+" ->
        parse_expr(rest, expr ++ [:add])

      "*" ->
        parse_expr(rest, expr ++ [:mul])

      "(" <> num ->
        {rexpr, rest} = parse_expr([num] ++ rest, [])

        parse_expr(rest, expr ++ [rexpr])

      ")" ->
        {expr, rest}

      num ->
        case Integer.parse(num) do
          {num, ")"} ->
            {expr ++ [num], rest}

          {num, ")" <> pars} ->
            pars = String.codepoints(pars)
            {expr ++ [num], pars ++ rest}

          {num, ""} ->
            parse_expr(rest, expr ++ [num])
        end
    end
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
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
  end
end
