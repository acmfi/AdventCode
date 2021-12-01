defmodule ReportRepair do
  @base_path Path.dirname(__ENV__.file)

  defp firstStar(input) do
    [elem|tail] = input
    res = Enum.filter(tail, fn x -> (elem + x) == 2020 end)
    if(Enum.empty?(res), do: firstStar(tail), else: elem * hd res)
  end

  defp recSecondStar(head, input) do
    case input do
      [] -> 0
      [_] -> 0
      [_|_] ->
            [elem|tail] = input
            res = Enum.filter(tail, fn x -> (head + elem + x) == 2020 end)
            if(Enum.empty?(res), do: recSecondStar(head, tail), else: head * elem * hd res)
    end
  end

  defp secondStar(input) do
    [elem|tail] = input
    res = recSecondStar(elem, tail)
    if(res == 0, do: secondStar(tail), else: res)
  end

  def main(fileName \\ "input.txt") do
    path = Path.join(@base_path, fileName)
    input = path 
          |> File.read!()
          |> String.split("\n")
          |> Enum.filter(&(&1 != ""))
          |> Enum.map(&(String.to_integer(&1)))
    IO.puts(firstStar(input))
    IO.puts(secondStar(input))
  end
end

ReportRepair.main()
