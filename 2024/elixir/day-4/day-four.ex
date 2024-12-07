defmodule XmasCounter do
  @straight 0
  @right 1
  @left -1
  @up -1
  @down 1

  # Updated to include rows, cols in arguments
  defp count_direction(puzzle, rows, cols, row, col, dir_row, dir_col, word) do
    len = String.length(word)

    check_chars = fn check_chars, r, c, i ->
      cond do
        i == len ->
          1

        r < 0 or r >= rows or c < 0 or c >= cols ->
          0

        Enum.at(Enum.at(puzzle, r), c) != String.at(word, i) ->
          0

        true ->
          check_chars.(check_chars, r + dir_row, c + dir_col, i + 1)
      end
    end

    check_chars.(check_chars, row, col, 0)
  end

  def part1() do
    puzzle =
      File.read!("wordsearch.txt")
      |> String.split("\n", trim: true)
      # now puzzle is a list of list of chars
      |> Enum.map(&String.graphemes/1)

    rows = length(puzzle)
    # since puzzle is now list of graphemes
    cols = length(hd(puzzle))

    result =
      Enum.reduce(0..(rows - 1), 0, fn i, acc ->
        Enum.reduce(0..(cols - 1), acc, fn j, inner_acc ->
          inner_acc +
            count_direction(puzzle, rows, cols, i, j, @right, @straight, "XMAS") +
            count_direction(puzzle, rows, cols, i, j, @left, @straight, "XMAS") +
            count_direction(puzzle, rows, cols, i, j, @straight, @up, "XMAS") +
            count_direction(puzzle, rows, cols, i, j, @straight, @down, "XMAS") +
            count_direction(puzzle, rows, cols, i, j, @right, @down, "XMAS") +
            count_direction(puzzle, rows, cols, i, j, @right, @up, "XMAS") +
            count_direction(puzzle, rows, cols, i, j, @left, @down, "XMAS") +
            count_direction(puzzle, rows, cols, i, j, @left, @up, "XMAS")
        end)
      end)

    IO.inspect(result, label: "Part 1 Result")
  end

  defp count_cross(puzzle, rows, cols, row, col) do
    result =
      count_direction(puzzle, rows, cols, row + @left, col + @up, @right, @down, "MAS") +
        count_direction(puzzle, rows, cols, row + @left, col + @down, @right, @up, "MAS") +
        count_direction(puzzle, rows, cols, row + @right, col + @up, @left, @down, "MAS") +
        count_direction(puzzle, rows, cols, row + @right, col + @down, @left, @up, "MAS")

    if result == 2, do: 1, else: 0
  end

  def part2() do
    puzzle =
      File.read!("wordsearch.txt")
      |> String.split("\n", trim: true)
      |> Enum.map(&String.graphemes/1)

    rows = length(puzzle)

    cols = length(hd(puzzle))

    result =
      Enum.reduce(0..(rows - 1), 0, fn i, acc ->
        Enum.reduce(0..(cols - 1), acc, fn j, inner_acc ->
          inner_acc +
            count_cross(puzzle, rows, cols, i, j)
        end)
      end)

    IO.inspect(result, label: "Part 2 Result")
  end
end

XmasCounter.part1()
XmasCounter.part2()
