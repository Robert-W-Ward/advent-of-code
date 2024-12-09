defmodule Board do
  @empty_cell "."
  @obstacle_cell "#"
  @start_cell "^"
  @type direction :: :north | :east | :south | :west
  @type position :: {integer(), integer()}
  @type visited :: MapSet.t()

  def position_map(input) do
    input
    |> Enum.with_index()
    |> Enum.map(fn {row, row_idx} ->
      Enum.with_index(row)
      |> Enum.map(fn {col, col_idx} ->
        {{col_idx, row_idx}, col}
      end)
    end)
    |> List.flatten()
    |> Enum.into(%{})
  end

  def solve(start_position, start_direction, positions) do
    {x, y} = start_position
    visited = MapSet.new()
    iterate(x, y, start_direction, visited, positions)
  end

  def iterate(x, y, direction, visited, positions) do
    # Get next position based on current position and direction of travel
    next_position =
      case direction do
        :north -> {x, y - 1}
        :east -> {x + 1, y}
        :south -> {x, y + 1}
        :west -> {x - 1, y}
      end

    {next_x, next_y} = next_position

    # Mark current location x,y as visited along with the direction we were headed
    new_visited = MapSet.put(visited, {{x, y}, direction})

    cond do
      MapSet.member?(visited, {{x, y}, direction}) ->
        "loop"

      next_y < 0 or next_y >= :math.sqrt(Kernel.map_size(positions)) ->
        new_visited

      next_x < 0 or next_x >= :math.sqrt(Kernel.map_size(positions)) ->
        new_visited

      true ->
        # Check if new position is a empty cell, obstacle or something else
        case Map.get(positions, next_position, :not_found) do
          # Move forward
          @empty_cell ->
            iterate(next_x, next_y, direction, new_visited, positions)

          @start_cell ->
            iterate(next_x, next_y, direction, new_visited, positions)

          @obstacle_cell ->
            # Encountered an obstacle: change direction and continue recursion
            new_direction = next_direction(direction)
            iterate(x, y, new_direction, new_visited, positions)

          _ ->
            # Reached map bounds
            new_visited
        end
    end
  end

  defp next_direction(:north), do: :east
  defp next_direction(:east), do: :south
  defp next_direction(:south), do: :west
  defp next_direction(:west), do: :north

  def swap_char(grid, x, y) do
    List.update_at(
      grid,
      y,
      &List.update_at(&1, x, fn char ->
        case char do
          "." -> "#"
          _ -> "^"
        end
      end)
    )
  end

  def part2(visited, grid) do
    visited
    |> MapSet.new(fn {{x, y}, _direction} -> {x, y} end)
    |> Enum.map(fn {x, y} ->
      # Maps over every position of the guards *acutal* patrol
      # Swaps each position with a new *obstacle"
      # solves this *new* board
      solve(swap_char(grid, x, y))
    end)
      # Counts the number of these *new* boards return *loop*
    |> Enum.count(&(&1 == "loop"))
  end
end

# Reading and parsing the map file
{:ok, contents} = File.read("map.txt")
matrix = contents |> String.split("\n", trim: true) |> Enum.map(&String.graphemes/1)

visited = Board.solve({51, 89}, :north, Board.position_map(matrix))
visited_no_dir = visited |> MapSet.new(fn {{x, y}, _direction} -> {x, y} end)
IO.puts("Part1 Answer #{MapSet.size(visited_no_dir)}")
IO.puts("Part2 Answer #{Board.part2(visited_no_dir,matrix)}")
