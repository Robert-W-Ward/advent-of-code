{:ok, contents} = File.read("sorting-rules.txt")
lines = String.split(contents, "\n")

rules =
  lines
  |> Enum.map(fn line -> String.split(line, "|") end)
  |> List.flatten()
  |> Enum.chunk_every(2)
  |> Enum.reduce(%{}, fn [key, value], acc ->
    key = String.to_integer(key)
    value = String.to_integer(value)

    Map.update(acc, key, [value], fn existing ->
      [value | existing]
    end)
  end)
  |> Enum.map(fn {k, v} -> {k, Enum.sort(v)} end)
  |> Enum.into(%{})

{:ok, contents} = File.read("lists-to-sort.txt")

in_order? = fn list ->
  index_map =
    list
    |> Enum.with_index()
    |> Enum.into(%{}, fn {val, idx} -> {val, idx} end)

  Enum.all?(list, fn x ->
    vals = Map.get(rules, x, [])

    Enum.all?(vals, fn y ->
      x_idx = Map.get(index_map, x)
      y_idx = Map.get(index_map, y)
      y_idx == nil or x_idx < y_idx
    end)
  end)
end

midpoint = fn list ->
  mid_idx = div(length(list), 2)
  Enum.at(list, mid_idx)
end

# Part 1
part1 =
  contents
  |> String.split("\n")
  |> Enum.map(fn sublist ->
    String.split(sublist, ",")
    |> Enum.map(fn elem ->
      String.to_integer(elem, 10)
    end)
  end)
  |> Enum.filter(in_order?)
  |> Enum.reduce(0, fn list, acc -> acc + midpoint.(list) end)

IO.puts("Part 1: #{part1}")

swap_elements = fn list, idx1, idx2 ->
  list
  |> List.replace_at(idx1, Enum.at(list, idx2))
  |> List.replace_at(idx2, Enum.at(list, idx1))
end

flatten_rules = fn rules ->
  rules
  |> Enum.flat_map(fn {key, values} ->
    Enum.map(values, fn value -> [key, value] end)
  end)
end

fix_list = fn list, rules ->
  Enum.reduce_while(rules, list, fn [a, b], acc_list ->
    idx1 = Enum.find_index(acc_list, &(&1 == a))
    idx2 = Enum.find_index(acc_list, &(&1 == b))

    if idx1 && idx2 && idx1 > idx2 do
      {:cont, swap_elements.(acc_list, idx1, idx2)}
    else
      {:cont, acc_list}
    end
  end)
end

fix_until_sorted = fn list, rules ->
  Stream.iterate(list, fn acc -> fix_list.(acc, rules) end)
  |> Enum.reduce_while(nil, fn current, prev ->
    if current == prev, do: {:halt, current}, else: {:cont, current}
  end)
end

# Part 2
part2 =
  contents
  |> String.split("\n")
  |> Enum.map(fn sublist ->
    String.split(sublist, ",")
    |> Enum.map(&String.to_integer/1)
  end)
  |> Enum.filter(fn x -> not in_order?.(x) end)
  |> Enum.map(fn list ->
    flattened_rules = flatten_rules.(rules)
    fix_until_sorted.(list, flattened_rules)
  end)
  |> Enum.reduce(0, fn list, acc -> acc + midpoint.(list) end)

IO.puts("Part 2: #{part2}")
