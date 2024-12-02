# sort two lists
# take the pair-wise absolute difference between each element of the lists
# total ALL of these differences to get the "total distance"
# Read file into a 2D array [[l1][l2]]
# sort both lists least to greatest
# pair off each element of l1 with each element of l2 i.e. zip
# take the abs(difference(l1[x],l2[y])) and keep a running total
# continue

# Part 1:
{:ok, contents} = File.read("list.txt")
lines = String.split(contents, "\n")

intermediate =
  lines
  |> Enum.flat_map(fn line ->
    String.split(line, "   ")
  end)
  |> Enum.map(fn ele ->
    String.to_integer(ele, 10)
  end)
  |> Enum.with_index()
  |> Enum.reduce({[], []}, fn {n, i}, {e, o} ->
    if rem(i, 2) == 0 do
      {[n | e], o}
    else
      {e, [n | o]}
    end
  end)
  |> (fn {evens, odds} ->
        {
          Enum.reverse(evens)
          |> Enum.sort(&(&2 > &1)),
          Enum.reverse(odds)
          |> Enum.sort(&(&2 > &1))
        }
      end).()

result =
  intermediate
  |> (fn {e, o} -> Enum.zip(e, o) end).()
  |> Enum.map(fn {a, b} ->
    abs(a - b)
  end)
  |> Enum.sum()

IO.puts("Part 1 simalarity score: #{result}")

# For each element in l1 count how many times it appears in l2
# then multiply the number by the occurence count for each element in l1 save this as l3
# sum each element in l3

result =
  intermediate
  |> (fn {e, o} ->
        Enum.map(e, fn i ->
          count = Enum.count(o, fn j -> j == i end)
          i * count
        end)
      end).()
  |> Enum.sum()

IO.puts("Part 2 simalarity score: #{result}")
