defmodule Solution do
  def ex1(path \\ "input") do
    path |> File.read!() |> String.to_charlist() |> minimize() |> length()
  end

  def ex2(path \\ "input") do
    path |> File.read!() |> String.to_charlist() |> polymer_combinations()
  end

  defguard unit_react(u1, u2) when abs(u1 - u2) == 32

  defp polymer_combinations(all_chars) do
    all_chars
    |> Stream.map(&char_upcase/1)
    |> Enum.uniq()
    |> Enum.map(fn char ->
      all_chars |> remove_char(char) |> minimize() |> length()
    end)
    |> Enum.min()
  end

  defp char_upcase(char), do: if(char - 32 < 65, do: char, else: char - 32)

  defp remove_char(chars, char) do
    Enum.filter(chars, fn c ->
      char != char_upcase(c)
    end)
  end

  defp minimize(list, acc \\ [])

  defp minimize([], acc), do: acc

  defp minimize([ch1, ch2 | rest], []) when unit_react(ch1, ch2) do
    minimize(rest, [])
  end

  defp minimize([ch1, ch2 | rest], [prev | acc]) when unit_react(ch1, ch2) do
    minimize([prev | rest], acc)
  end

  defp minimize([ch1 | rest], acc), do: minimize(rest, [ch1 | acc])
end
