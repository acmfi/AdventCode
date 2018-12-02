defmodule IronjanowarTest do
  use ExUnit.Case
  doctest Ironjanowar

  test "greets the world" do
    assert Ironjanowar.hello() == :world
  end
end
