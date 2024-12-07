# use a REGEX to filter through all the text of the file and sum up their values if they are proper mul expressions
# Part 1
{:ok, contents} = File.read("corrupted-memory.txt")

pattern = ~r/mul\(\d+,\d+\)+/

matches = Regex.scan(pattern, contents)

flattened = Enum.map(matches, fn [match] -> match end)

result =
  flattened
  |> Enum.map(fn mul ->
    Regex.scan(~r/\d+/, mul)
    |> Enum.flat_map_reduce(1, fn [val], acc ->
      num = String.to_integer(val)

      {[num], acc * num}
    end)
  end)
  |> Enum.reduce(0, fn {_m, p}, acc ->
    p + acc
  end)

IO.puts("Part 1 Answer: #{result}")
# Part 2

combined_pattern = ~r/mul\(\d+,\d+\)|do\(\)|don't\(\)/

matches = Regex.scan(combined_pattern, contents)

flattended = Enum.map(matches, fn [match] -> match end)

result =
  flattended
  |> Enum.reduce({false, []}, fn
    "don't()", {_, acc} ->
      {true, acc}

    "do()", {true, acc} ->
      {false, acc}

    "do()", {false, acc} ->
      {false, acc}

    _elem, {true, acc} ->
      {true, acc}

    elem, {false, acc} ->
      {false, [elem | acc]}
  end)
  |> (fn {_, list} -> Enum.reverse(list) end).()
  |> Enum.map_reduce([], fn item, acc ->
    nums =
      Regex.scan(~r/\d+/, item)
      |> List.flatten()
      |> Enum.map(&String.to_integer/1)

    product = Enum.reduce(nums, 1, &*/2)

    {product, [product | acc]}
  end)

sum = elem(result, 1) |> Enum.sum()

IO.puts("Part 2 answer: #{sum}")
