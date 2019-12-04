defmodule AdventCode do
  def get_input() do
    172_930..683_082
  end

  @doc """
  - It is a six-digit number.
  - The value is within the range given in your puzzle input.
  - Two adjacent digits are the same (like 22 in 122345).
  - Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
  """
  def day4() do
    get_input() |> Enum.filter(fn number -> validate(number) == {:ok, :valid} end) |> Enum.count()
  end

  @doc """
  Validates a number

  iex> AdventCode.validate(555555)
  {:ok, :valid}
  iex> AdventCode.validate(556780)
  {:error, "Digits decrease from left to right"}
  iex> AdventCode.validate(456789)
  {:error, "Number has no adjacent digits repeated"}
  """
  def validate(number) do
    str = number |> Integer.to_string()

    cond do
      String.length(str) != 6 -> {:error, "Is not a 6 digit number"}
      not (number in get_input()) -> {:error, "Number is not in range"}
      not adjacent_repeated?(str) -> {:error, "Number has no adjacent digits repeated"}
      not digits_never_decrease?(str) -> {:error, "Digits decrease from left to right"}
      true -> {:ok, :valid}
    end
  end

  @doc """
  Checks if number has adjacents repeated

  iex> AdventCode.adjacent_repeated?("11234")
  true
  iex> AdventCode.adjacent_repeated?("1234")
  false
  iex> AdventCode.adjacent_repeated?("567899")
  true
  """
  def adjacent_repeated?([h | rest]), do: adjacent_repeated?(rest, h)

  def adjacent_repeated?(str) do
    str |> String.to_charlist() |> adjacent_repeated?(str)
  end

  def adjacent_repeated?([h | _rest], h), do: true
  def adjacent_repeated?([h | rest], _l), do: adjacent_repeated?(rest, h)
  def adjacent_repeated?([], _), do: false

  @doc """
  Checks if digits never decrease from left to right

  iex> AdventCode.digits_never_decrease?("1234")
  true
  iex> AdventCode.digits_never_decrease?("1123")
  true
  iex> AdventCode.digits_never_decrease?("1231")
  false
  """
  def digits_never_decrease?([h | rest]), do: digits_never_decrease?(rest, h)

  def digits_never_decrease?(str) do
    str |> String.to_charlist() |> digits_never_decrease?()
  end

  def digits_never_decrease?([h | rest], prev) when prev <= h, do: digits_never_decrease?(rest, h)
  def digits_never_decrease?([], _), do: true
  def digits_never_decrease?(_, _), do: false
end
