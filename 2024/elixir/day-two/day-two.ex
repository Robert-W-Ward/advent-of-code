# Check that each line/list of numbers is monotonically increasing or decreasing
# then check if the per element increase/decrease is at least 1 and at most 3
defmodule MonotonicityChecker do
  def is_monotonic(list) do
    Enum.all?(1..(length(list) - 1), fn i ->
      list[i] <= list[i + 1] || list[i] >= list[i + 1]
    end)
  end
end

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
    |> Enum.all?(fn [a, b] ->
      is_increasing =
        Enum.chunk_every(sublist, 2, 1, :discard)
        |> Enum.all?(fn [a, b] -> a <= b end)

      is_decreasing =
        Enum.chunk_every(sublist, 2, 1, :discard)
        |> Enum.all?(fn [a, b] -> a >= b end)

      is_increasing or is_decreasing
    end)
  end)

IO.puts(length(cleaned))
IO.puts(length(monotonic_lists))
IO.inspect(monotonic_lists, charlists: :as_lists)
