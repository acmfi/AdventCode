defmodule Tree do
  defmodule Node do
    defstruct child_n: 0, metadata_n: 0, childs: [], metadata: [], value: 0

    def new(cn, mn, cs, m) do
      node = %__MODULE__{child_n: cn, metadata_n: mn, childs: cs, metadata: m}
      value = calculate_value(node)
      %{node | value: value}
    end

    def calculate_value(%__MODULE__{childs: [], metadata: m}), do: Enum.sum(m)

    def calculate_value(%__MODULE__{childs: childs, metadata: metadata}) do
      Enum.reduce(metadata, 0, fn met, sum ->
        sum + child_value(met, childs)
      end)
    end

    defp child_value(0, _), do: 0

    defp child_value(n, childs) do
      case Enum.at(childs, n - 1) do
        nil -> 0
        child -> child.value
      end
    end
  end

  alias Tree.Node

  defimpl String.Chars, for: Tree.Node do
    def to_string(node) do
      childs = node.childs |> Enum.map(&"#{&1}") |> Enum.join(", ")
      metadata = Enum.join(node.metadata, ", ")

      "#Node<child_n:#{node.child_n} meta_n:#{node.metadata_n} metadata:[#{metadata}] childs:[#{
        childs
      }]>"
    end
  end

  def ex(input \\ "input_test") do
    input
    |> File.read!()
    |> String.split()
    |> build_tree()
    |> elem(1)
    |> IO.inspect()
    |> sum_metadata()
    |> IO.inspect()
  end

  defp build_tree(input)

  defp build_tree([]), do: nil

  defp build_tree(input) do
    {n_childs, n_metadata, input} = header(input)

    {input, childs} =
      0
      |> Range.new(n_childs)
      |> Stream.drop(1)
      |> Enum.reduce(
        {input, []},
        fn _, {input, childs} ->
          {input, child} = build_tree(input)
          {input, childs ++ [child]}
        end
      )

    {input, metadata} = metadata(input, n_metadata)

    n = Node.new(n_childs, n_metadata, childs, metadata)

    {input, n}
  end

  defp sum_metadata(%Node{metadata: metadata, childs: childs}) do
    ms = Enum.sum(metadata)
    childs_sum = childs |> Enum.map(&sum_metadata/1) |> Enum.sum()
    ms + childs_sum
  end

  defp header([n_childs, n_metadata | rest]),
    do: {to_integer!(n_childs), to_integer!(n_metadata), rest}

  defp to_integer!(string), do: string |> Integer.parse() |> elem(0)

  defp metadata(input, n, result \\ [])
  defp metadata(input, 0, result), do: {input, result}

  defp metadata(input, n, result) do
    {m, input} = consume(input)

    metadata(input, n - 1, [m | result])
  end

  defp consume(input) do
    {elem, rest} = List.pop_at(input, 0)
    {to_integer!(elem), rest}
  end
end
