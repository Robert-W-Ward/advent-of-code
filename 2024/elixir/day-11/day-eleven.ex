# stone.value == 0 replace with a whose stone.value == 1
# stone.digit_count.parity == even replace by two stones following these rules:
#   left half of digits on new left stone (trim leading and trailing zeros)
#   right half of digits on new right stone (trim leading and trailing zeros)
# if no other rules apply new_stone.value = (old_stone.value * 2024)
# No matter what the order is preserved
{:ok, contents} = File.read("input.txt")
{:ok, smaller} = File.read("smaller.txt")

list =
  contents

# |> String.split()

# part 1
# Enum.reduce(0..24, list, fn _i, new_list ->
#   new_list =
#     Enum.flat_map_reduce(new_list, [], fn num, acc ->
#       len_of_num = String.length(num)
#       parity_is_even = rem(len_of_num, 2) == 0

#       cond do
#         num == "0" ->
#           result = ["1"]
#           {result, acc ++ result}

#         parity_is_even == true ->
#           {left, right} = String.split_at(num, div(len_of_num, 2))

#           trimmed_l = if (trimmed = String.trim_leading(left, "0")) == "", do: "0", else: trimmed
#           trimmed_r = if (trimmed = String.trim_leading(right, "0")) == "", do: "0", else: trimmed

#           result = [trimmed_l, trimmed_r]
#           {result, acc ++ result}

#         true ->
#           new_value = String.to_integer(num) * 2024
#           result = [Integer.to_string(new_value)]
#           {result, acc ++ result}
#       end
#     end)
#     |> elem(0)
# end)
# |> length()
# |> IO.inspect()

# Part 2
defmodule Optimized do
  def digit_length_parity(num) when num > 0 do
    len = Integer.digits(num) |> length()
    rem(len, 2) == 0
  end

  def split_number(num) do
    divisor = 10 ** (div(trunc(:math.log10(num)), 2) + 1)
    {div(num, divisor), rem(num, divisor)}
  end

  def blink(m, 0), do: m

  def blink(m, iterations) do
    m
    |> Enum.reduce(%{}, fn {num, count}, acc ->
      cond do
        num == 0 ->
          acc
          |> Map.update(1, count, &(&1 + count))

        digit_length_parity(num) ->
          {left, right} = split_number(num)

          acc
          |> Map.update(left, count, &(&1 + count))
          |> Map.update(right, count, &(&1 + count))

        true ->
          Map.update(acc, num * 2024, count, &(&1 + count))
      end
    end)
    |> blink(iterations - 1)
  end

  def solve(input, num_blinks) do
    numbers =
      input
      |> String.split(" ")
      |> Enum.map(&String.to_integer/1)

    numbers
    |> Enum.map(fn num -> {num, Enum.count(numbers, &(&1 == num))} end)
    |> Enum.uniq()
    |> Enum.group_by(&elem(&1, 0))
    |> Map.new(fn {num, [{_, count}]} -> {num, count} end)
    |> blink(num_blinks)
    |> Enum.reduce(0, fn {_k, v}, acc -> acc + v end)
  end
end

Optimized.solve(list, 75) |> IO.puts()
# for each stone (number) check each condition in turn
# update list with the result of the current stones action
# continue to end of list
# count lenght of list
