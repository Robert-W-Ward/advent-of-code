{:ok, contents} = File.read("calibrations.txt")
lines = contents |> String.split("\r\n", trim: true)

defmodule Calibrator do
  def calibrate(cur, remaining, goal) do
    case remaining do
      [] ->
        cur == goal

      _ ->
        [next | rest] = remaining
        sum = cur + next
        prod = cur * next

        concat =
          String.to_integer(Integer.to_string(cur) <> Integer.to_string(next))

        calibrate(sum, rest, goal) or calibrate(prod, rest, goal) or calibrate(concat, rest, goal)
    end
  end
end

c =
  Enum.map(lines, fn line ->
    split = String.split(line)

    Enum.map(split, fn elem ->
      String.trim(elem, ":") |> String.to_integer()
    end)
  end)

# for each pair of elments perform *operation*
Enum.map(c, fn line ->
  [goal | numbers] = line

  case Calibrator.calibrate(hd(numbers), tl(numbers), goal) do
    true -> goal
    false -> 0
  end
end)
|> Enum.sum()
|> IO.inspect()

# If operation would go over goal switch to other operation and retry iteration with this one, if it still goes over list can't be fixed
# Else continue to next pair with previous operation
