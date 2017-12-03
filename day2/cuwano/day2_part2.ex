defmodule AdventCode do
  def check(elem, list) do
    case Enum.filter(list, fn x -> rem(x, elem) == 0 end) do
      [] -> :nope
      [x] -> {:found, x, elem}
    end
  end

  def getNumbers([h|rest]) do
    case check(h, rest) do
      {:found, a, b} -> {a, b}
      :nope -> getNumbers(rest ++ [h])
    end
  end

  def checkRow(row, acc) do
    with {a, b} <- getNumbers(row),
         c <- a/b do
      acc + c
    end
  end

  def checksum(x), do: List.foldl(x, 0, &checkRow/2)
end
