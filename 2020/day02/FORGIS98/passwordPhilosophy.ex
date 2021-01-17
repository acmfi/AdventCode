defmodule PasswordPhilosophy do
  @base_path Path.dirname(__ENV__.file)

  defp firstStar(input) do
    res = Enum.filter(input, &(_firstStar(&1))) |> Enum.count()
    IO.puts(res)
  end

  defp _firstStar(data) do
    letra = elem(data, 0) |> Map.keys() |> hd
    min = elem(data,0) |> Map.fetch!(letra) |> elem(0)
    max = elem(data,0) |> Map.fetch!(letra) |> elem(1)

    times = Regex.scan(~r/#{letra}/, elem(data, 1)) |> Enum.count()
    if(times >= min and times <= max) do true else false end
  end

  defp parseLine(line) do
    [rango, letra, password] = String.split(line, " ")
    [min, max] = String.split(rango, "-") |> Enum.map(&String.to_integer/1)
    letra = String.trim(letra, ":")
    {%{letra => {min, max}}, password}
  end

  def main(fileName \\ "input.txt") do
    path = Path.join(@base_path, fileName)
    input = path 
          |> File.read!()
          |> String.split("\n")
          |> Enum.filter(&(&1 != ""))
          |> Enum.map(&parseLine/1)
    IO.puts("Primera Estrella")
    firstStar(input)
  end
end

PasswordPhilosophy.main()
