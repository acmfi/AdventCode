defmodule AdventCode do
  def sum([x|_] = xs) do
    megasum xs, x
  end

  def megasum([x,x|xs], h), do: x + megasum([x|xs], h)
  def megasum([_,x|xs], h), do: megasum([x|xs], h)
  def megasum([x], x), do: x
  def megasum(_, _), do: 0
end
