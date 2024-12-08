defmodule Board do
  @empty_cell "."
  @obstacle_cell "#"
  @type direction :: :north | :east | :south | :west
  @type position :: {integer(), integer()}
  @type visited :: MapSet.t()
  defstruct state: [], direction: :north, visited: MapSet.new()

  def solve(start_position, direction, positions) do
    visited = MapSet.new([start_position])
    iterate(start_position, direction, visited, positions)
  end

  defp iterate(current_position, current_direction, visited, positions) do
    {new_position, new_direction, updated_visited} =
      step(current_position, current_direction, visited, positions)

    if new_position == current_position and MapSet.size(updated_visited) > 1 do
      IO.puts("Finished! Total spaces visited: #{MapSet.size(updated_visited)}")
    else
      iterate(new_position, new_direction, updated_visited, positions)
    end
  end

  @spec step(position(), direction(), MapSet.t(), map()) :: {position(), direction(), MapSet.t()}
  def step(current_position, current_direction, visited, positions) do
    {x, y} = current_position

    new_position =
      case current_direction do
        :north -> {x, y + 1}
        :east -> {x + 1, y}
        :south -> {x, y - 1}
        :west -> {x - 1, y}
      end

    case Map.get(positions, new_position, :not_found) do
      @obstacle_cell ->
        new_directon = next_direction(current_direction)
        {current_position, new_directon, visited}

      _ ->
        updated_visited = MapSet.put(visited, new_position)
        {new_position, current_direction, updated_visited}
    end
  end

  # Get the next direction clockwise
  defp next_direction(:north), do: :east
  defp next_direction(:east), do: :south
  defp next_direction(:south), do: :west
  defp next_direction(:west), do: :north
end

{:ok, contents} = File.read("map.txt")
matrix = String.split(contents, "\n") |> Enum.map(fn row -> String.graphemes(row) end)

positions =
  matrix
  # Rows of the matrix -> tuples wit {list , row_idx}
  |> Enum.with_index()
  # maps of these new tuples
  |> Enum.map(fn {row, row_idx} ->
    Enum.with_index(row)
    |> Enum.map(fn {col, col_idx} ->
      {{row_idx, col_idx}, col}
    end)
  end)
  |> List.flatten()
  |> Enum.sort(fn {{row1, col1}, _}, {{row2, col2}, _} ->
    {row1, col1} <= {row2, col2}
  end)
  # |> IO.inspect(limit: :infinity)
  |> Enum.into(%{})

start_position =
  positions
  |> Enum.find(fn {_key, value} -> value == "^" end)
  |> case do
    {key, _value} -> key
    nil -> :not_found
  end

Board.solve(start_position, :north, positions)
