defmodule Solve do
  defmodule Group do
    defstruct id: nil,
              side: nil,
              count: 0,
              # Health
              points: 0,
              attack: 0,
              attack_type: nil,
              initiative: 0,
              weaknesses: [],
              immunities: []

    def power(%__MODULE__{} = g) do
      g.count * g.attack
    end

    def damage(%__MODULE__{} = g1, %__MODULE__{} = g2) do
      imm = g1.attack_type in g2.immunities
      weak = g1.attack_type in g2.weaknesses

      power = Group.power(g1)

      case {imm, weak} do
        {true, _} -> 0
        {_, true} -> power * 2
        _ -> power
      end
    end

    def attack(%__MODULE__{} = a, %__MODULE__{} = d) do
      dmg = damage(a, d)
      kill = div(dmg, d.points)
      count = max(d.count - kill, 0)

      %{d | count: count}
    end
  end

  alias __MODULE__.Group

  def ex(path \\ "input_test") do
    groups = path |> File.read!() |> parse()

    s1 = ex1(groups)
    IO.puts("Star 1: #{s1}")

    s2 = ex2(groups)
    IO.puts("Star 2: #{s2}")
  end

  defp ex1(groups) do
    {_, last_groups} = combat(groups)
    count_standing(last_groups)
  end

  defp ex2(groups) do
    groups |> cheat_combat(1) |> count_standing()
  end

  defp count_standing(groups) do
    groups |> Map.values() |> Stream.map(& &1.count) |> Enum.sum()
  end

  defp cheat_combat(groups, b) do
    IO.puts("Boost: #{b}")
    cheats = boost(groups, b)
    {winner, last_groups} = combat(cheats)

    case winner do
      :immune ->
        last_groups

      _ ->
        cheat_combat(groups, b + 1)
    end
  end

  defp boost(groups, n) do
    groups
    |> Stream.map(fn {id, g} ->
      if g.side == :immune do
        {id, %{g | attack: g.attack + n}}
      else
        {id, g}
      end
    end)
    |> Enum.into(%{})
  end

  defp combat(groups) do
    if group_winner?(groups) do
      winner = groups |> Enum.at(0) |> elem(1) |> Map.get(:side)

      {winner, groups}
    else
      after_attack = groups |> select_targets() |> attack(groups)

      if after_attack == groups do
        # Woops, we are not dealing any damage, we won't be able to continue :(
        {:none, after_attack}
      else
        after_attack |> combat()
      end
    end
  end

  defp group_winner?(groups) do
    alive_groups = groups |> Map.values() |> Enum.group_by(& &1.side) |> Enum.count()

    alive_groups == 1
  end

  defp attack(targets, groups) do
    order = groups |> Map.values() |> Enum.sort_by(& &1.initiative, &>=/2) |> Enum.map(& &1.id)

    order
    |> do_attack(groups, targets)
    |> Stream.filter(fn {_, g} -> g.count > 0 end)
    |> Enum.into(%{})
  end

  defp do_attack([], groups, _), do: groups

  defp do_attack([attacker | rest], groups, targets) do
    with %Group{count: c} = ag when c > 0 <- groups[attacker],
         dealt_to when not is_nil(dealt_to) <- targets[attacker] do
      dg = groups[dealt_to]

      dg = Group.attack(ag, dg)

      groups = Map.put(groups, dg.id, dg)
      do_attack(rest, groups, targets)
    else
      _ -> do_attack(rest, groups, targets)
    end
  end

  defp select_targets(groups) do
    groups
    |> Map.values()
    |> Enum.sort_by(
      fn g ->
        {Group.power(g), g.initiative}
      end,
      &>=/2
    )
    |> Enum.reduce({%{}, MapSet.new()}, &select_target_group(&1, groups, &2))
    |> elem(0)
  end

  defp select_target_group(group, all_groups, {selects, not_avail}) do
    enemies =
      all_groups
      |> Map.values()
      |> Stream.filter(&(&1.side != group.side))
      |> Stream.filter(&(not MapSet.member?(not_avail, &1.id)))

    targets =
      enemies
      |> Stream.map(fn ge ->
        dmg = Group.damage(group, ge)
        pw = Group.power(ge)
        {ge.id, {dmg, pw, ge.initiative}}
      end)
      |> Stream.filter(fn {_, {dmg, _, _}} ->
        dmg > 0
      end)
      |> Enum.sort_by(&elem(&1, 1), &>=/2)

    case targets do
      [] ->
        {selects, not_avail}

      [{tgt, _} | _] ->
        s = Map.put(selects, group.id, tgt)
        a = MapSet.put(not_avail, tgt)
        {s, a}
    end
  end

  defp parse(t) do
    [immune, infection] = t |> String.split("\n\n")

    imm =
      immune
      |> String.split("\n")
      |> Stream.drop(1)
      |> filter_empty
      |> Stream.with_index()
      |> Enum.map(&parse_group(&1, :immune))

    inf =
      infection
      |> String.split("\n")
      |> Stream.drop(1)
      |> filter_empty
      |> Stream.with_index()
      |> Enum.map(&parse_group(&1, :infection))

    (imm ++ inf) |> Stream.map(&{&1.id, &1}) |> Enum.into(%{})
  end

  defp filter_empty(s), do: Stream.filter(s, &(&1 != ""))

  defp parse_group({line, i}, group) do
    words = String.split(line, " ")
    count = int(Enum.at(words, 0))
    points = int(Enum.at(words, 4))

    init = int(Enum.at(words, -1))
    attack_type = atom(Enum.at(words, -5))
    attack = int(Enum.at(words, -6))

    {weaknesses, immunities} = parse_weaks_imms(line)

    id = "#{group}_#{i + 1}"

    %Group{
      id: id,
      side: group,
      count: count,
      points: points,
      attack: attack,
      attack_type: attack_type,
      initiative: init,
      weaknesses: weaknesses,
      immunities: immunities
    }
  end

  @imm_regex ~r/.+\((.+)\)/
  defp parse_weaks_imms(line) do
    case Regex.run(@imm_regex, line) do
      nil ->
        {[], []}

      [_, t] ->
        conds = t |> String.split("; ") |> Stream.map(&parse_cond/1) |> Enum.into(%{})

        {Map.get(conds, :weaks, []), Map.get(conds, :ims, [])}
    end
  end

  defp parse_cond("weak to " <> weaks) do
    weaks = weaks |> String.split(", ") |> Enum.map(&atom/1)
    {:weaks, weaks}
  end

  defp parse_cond("immune to " <> ims) do
    ims = ims |> String.split(", ") |> Enum.map(&atom/1)
    {:ims, ims}
  end

  defp int(x), do: String.to_integer(x)
  defp atom(x), do: String.to_atom(x)
end
