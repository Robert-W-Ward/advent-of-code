# Check that each line/list of numbers is monotonically increasing or decreasing
# then check if the per element increase/decrease is at least 1 and at most 3
# Part one: A SAFE report = monotonically increasing or decreasing by at least 1 and at most 3
{:ok, contents} = File.read("reports.txt")
lines = String.split(contents, "\n")

cleaned =
  lines
  |> Enum.map(fn line ->
    line
    |> String.split(" ")
    # Remove empty strings
    |> Enum.filter(&(&1 != ""))
    # Convert to integers
    |> Enum.map(&String.to_integer/1)
  end)

monotonic_lists =
  cleaned
  |> Enum.filter(fn sublist ->
    Enum.chunk_every(sublist, 2, 1, :discard)
    |> Enum.all?(fn [_a, _b] ->
      is_increasing =
        Enum.chunk_every(sublist, 2, 1, :discard)
        |> Enum.all?(fn [a, b] -> a <= b end)

      is_decreasing =
        Enum.chunk_every(sublist, 2, 1, :discard)
        |> Enum.all?(fn [a, b] -> a >= b end)

      is_increasing or is_decreasing
    end)
  end)

monotonic_lists_strict =
  monotonic_lists
  |> Enum.filter(fn sublist ->
    Enum.chunk_every(sublist, 2, 1, :discard)
    |> Enum.all?(fn [_a, _b] ->
      diff_at_least_one =
        Enum.chunk_every(sublist, 2, 1, :discard)
        |> Enum.all?(fn [a, b] -> abs(a - b) >= 1 end)

      diff_at_most_three =
        Enum.chunk_every(sublist, 2, 1, :discard)
        |> Enum.all?(fn [a, b] -> abs(a - b) <= 3 end)

      diff_at_least_one and diff_at_most_three
    end)
  end)

IO.puts("Part one number of 'safe' reports #{length(monotonic_lists_strict)}")

# SAFE REPORT = monotoncially increasing or decreasing by at least 1 and at most 1,
# BUT if you can remove 1 number to make it monotonic and it still is increasing or decreasing by at least 1 and at most 3 you can consider it still "SAFE
# Same as before but you can remove a SINGLE number to make it safe
not_safe =
  cleaned
  |> Enum.filter(fn sublist ->
    if(!Enum.member?(monotonic_lists_strict, sublist)) do
      sublist
    end
  end)

dampened_safe =
  not_safe
  |> Enum.filter(fn sublist ->
    Enum.with_index(sublist)
    |> Enum.any?(fn {_, index} ->
      List.delete_at(sublist, index)
      |> (fn new_list ->
            is_increasing =
              Enum.chunk_every(new_list, 2, 1, :discard)
              |> Enum.all?(fn [a, b] -> a <= b end)

            is_decreasing =
              Enum.chunk_every(new_list, 2, 1, :discard)
              |> Enum.all?(fn [a, b] -> a >= b end)

            diff_at_least_one =
              Enum.chunk_every(new_list, 2, 1, :discard)
              |> Enum.all?(fn [a, b] -> abs(a - b) >= 1 end)

            diff_at_most_three =
              Enum.chunk_every(new_list, 2, 1, :discard)
              |> Enum.all?(fn [a, b] -> abs(a - b) <= 3 end)

            (is_increasing or is_decreasing) and (diff_at_least_one and diff_at_most_three)
          end).()
    end)
  end)

IO.puts(length(dampened_safe))
