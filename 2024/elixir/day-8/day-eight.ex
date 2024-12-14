{:ok, contents} = File.read("map.txt")
lines = String.split(contents, "\n", trim: true) |> Enum.map(&String.graphemes(&1))

# Antinodes need
# 1. To be perfectly inline with two antennas of the same frequency (two of the same letters),
# but only when one of the antenna is twice as far away as the other
# (for any pair of letters there are two antinodes one on either side of them)
euclidean = fn p1, p2 ->
  x1 = elem(p1, 0)
  x2 = elem(p2, 0)
  y1 = elem(p1, 1)
  y2 = elem(p2, 1)

  x_part = :math.pow(x2 - x1, 2)
  y_part = :math.pow(y2 - y1, 2)
  :math.sqrt(x_part + y_part)
end

position_map =
  lines
  |> Enum.with_index()
  |> Enum.map(fn {row, row_idx} ->
    Enum.with_index(row)
    |> Enum.map(fn {col, col_idx} ->
      {{col_idx, row_idx}, col}
    end)
    |> Enum.sort()
  end)
  |> List.flatten()
  |> Enum.group_by(fn {{_x, _y}, char} -> char end, fn {{x, y}, _char} -> {x, y} end)

position_map_inverse =
  lines
  |> Enum.with_index()
  |> Enum.map(fn {row, row_idx} ->
    Enum.with_index(row)
    |> Enum.map(fn {col, col_idx} ->
      {{col_idx, row_idx}, col}
    end)
    |> Enum.sort()
  end)
  |> List.flatten()
  |> Enum.into(%{})

antinodes = MapSet.new()

count_antinodes = fn list, inverse, part ->
  combos =
    for a <- list, b <- list, a != b do
      {a, b}
    end

  Enum.flat_map(combos, fn {{x1, y1}, {x2, y2}} ->
    dx = x1 - x2
    dy = y1 - y2

    case part do
      :part1 ->
        [{x2 - dx, y2 - dy}, {x1 + dx, y1 + dy}]

      :part2 ->
        d1 =
          Stream.iterate({x2, y2}, fn {x, y} -> {x - dx, y - dy} end)
          |> Enum.take_while(fn {x, y} -> Map.get(inverse, {x, y}, false) end)

        d2 =
          Stream.iterate({x1, y1}, fn {x, y} -> {x + dx, y + dy} end)
          |> Enum.take_while(fn {x, y} -> Map.get(inverse, {x, y}, false) end)

        d1 ++ d2
    end
  end)
  |> Enum.filter(fn {x, y} ->
    Map.get(position_map_inverse, {x, y}, false)
  end)
end

position_map
|> Map.keys()
|> Enum.flat_map(fn char ->
  case char do
    "." ->
      []

    _ ->
      count_antinodes.(position_map[char], position_map_inverse, :part2)
  end
end)
|> Enum.uniq()
|> Enum.count()
|> IO.inspect()
