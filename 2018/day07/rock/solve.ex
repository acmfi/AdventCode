defmodule Solve do
  defmodule Step do
    defstruct needs: 0, childs: [], name: nil

    def new(name), do: %__MODULE__{name: name}

    def add_need(step), do: %{step | needs: step.needs + 1}
    def rm_need(step), do: %{step | needs: step.needs - 1}

    def add_child(step, name), do: %{step | childs: step.childs ++ [name]}
  end

  alias __MODULE__.Step

  defmodule Solver do
    use GenServer

    @workers 5

    def init(steps) do
      {:ok, %{steps: steps, workers: @workers, caller: nil, start: nil}}
    end

    def solve(pid, caller) do
      send(pid, {:start_solve, caller})
    end

    def handle_info({:start_solve, caller}, state) do
      start = DateTime.utc_now()
      new_state = %{state | caller: caller, start: start}
      handle_info(:solve, new_state)
    end

    def handle_info(:finish, %{caller: caller, start: start}) do
      endt = DateTime.utc_now()
      diff = (DateTime.diff(endt, start, :milliseconds) / 100) |> Float.floor()
      # diff = DateTime.diff(endt, start)
      send(caller, {:end, diff})
      {:stop, :normal, %{}}
    end

    def handle_info(:solve, %{steps: steps, workers: workers} = state) do
      {steps, workers} = process_work(steps, workers, self())

      {:noreply, %{state | steps: steps, workers: workers}}
    end

    def handle_info({:end, step}, %{steps: steps, workers: workers} = state) do
      IO.puts("PROCESSED: #{step.name}")
      steps = ends(steps, step)

      msg =
        case Enum.count(steps) do
          0 -> :finish
          _ -> :solve
        end

      send(self(), msg)

      workers = min(workers + 1, @workers)
      {:noreply, %{state | steps: steps, workers: workers}}
    end

    def handle_info(_, state), do: {:noreply, state}

    defp process_work(steps, workers, pid) do
      to_work = steps |> available() |> Stream.take(workers) |> Enum.map(&elem(&1, 1))
      avail_workers = max(workers - Enum.count(to_work), 0)

      Enum.each(to_work, fn step ->
        sleep = sleep_time(step.name)
        IO.puts("PROCESSING: #{step.name} -> #{sleep / 1000}")
        Process.send_after(pid, {:end, step}, sleep)
      end)

      names = Enum.map(to_work, & &1.name)
      {Map.drop(steps, names), avail_workers}
    end

    defp available(steps) do
      steps
      |> Stream.filter(fn {_, value} -> value.needs == 0 end)
      |> Enum.sort_by(&elem(&1, 0))
    end

    @unit 100
    @base 60
    defp sleep_time(name) do
      ((String.to_charlist(name) |> Enum.at(0)) - 64 + @base) * @unit
    end

    defp ends(steps, step) do
      childs =
        step.childs
        |> Enum.map(fn c ->
          child = steps |> Map.get(c) |> Step.rm_need()
          {c, child}
        end)
        |> Enum.into(%{})

      Map.merge(steps, childs)
    end
  end

  alias __MODULE__.Solver

  def ex(input \\ "input_test") do
    graph =
      input |> File.read!() |> String.trim() |> String.split("\n", strip: true) |> build_graph()

    {:ok, pid} = GenServer.start_link(Solver, graph)

    Solver.solve(pid, self())

    receive do
      {:end, time} -> IO.puts("Finished work in: #{time}")
    end
  end

  defp build_graph(input, steps \\ %{})

  defp build_graph([], steps), do: steps

  defp build_graph([line | rest], steps) do
    [_, name, _, _, _, _, _, child_n | _] = String.split(line)
    parent = Map.get(steps, name, Step.new(name))
    child = Map.get(steps, child_n, Step.new(child_n))

    {child, parent} = needs(child, parent)

    steps = Map.merge(steps, %{name => parent, child_n => child})
    build_graph(rest, steps)
  end

  defp needs(child, parent) do
    child = Step.add_need(child)
    parent = Step.add_child(parent, child.name)
    {child, parent}
  end
end
