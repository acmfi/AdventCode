defmodule AdventCodeTest do
  use ExUnit.Case
  doctest AdventCode

  test "greets the world" do
    assert AdventCode.hello() == :world
  end
end
