# Gracias @rockneurotiko te he copiado la lectura de archivos ^_^
defmodule ReportRepair do
  @base_path Path.dirname(__ENV__.file)

  defp firstStar(input) do
    [elem|tail] = input
    res = Enum.filter(tail, fn x -> (elem + x) == 2020 end)
    if(Enum.empty?(res), do: firstStar(tail), else: elem * hd res)
  end

  def main(fileName \\ "input.txt") do
    path = Path.join(@base_path, fileName)
    input = path 
            |> File.read!() 
            |> String.split("\n") 
            |> Enum.filter(&(&1 != ""))
            |> Enum.map(&(String.to_integer(&1)))
    IO.puts(firstStar(input))
  end
end

ReportRepair.main()
