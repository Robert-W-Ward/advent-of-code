# find ALL instances of 'XMAS' in the word search they can be horizontal, vertical, diagonal, written backwards, or even overlapping other words.

{:ok, contents} = File.read("wordsearch.txt")

defmodule XmasCounter do
  @directions [
    # Horizontal: right
    {1, 0},
    # Horizontal: left
    {-1, 0},
    # Vertical: down
    {0, 1},
    # Vertical: up
    {0, -1},
    # Diagonal: down-right
    {1, 1},
    # Diagonal: up-right
    {1, -1},
    # Diagonal: down-left
    {-1, 1},
    # Diagonal: up-left
    {-1, -1}
  ]

  def count_xmas(input, width) do
    grid = parse_to_grid(input, width)
    total_count = count_all_directions(grid, "XMAS")
    IO.puts("Total XMAS occurences: #{total_count}")
  end

  defp parse_to_grid(input, width) do
    input
    |> String.graphemes()
    |> Enum.chunk_every(width)
  end

  defp count_all_directions(grid, word) do
    Enum.reduce(@directions, 0, fn {dx, dy}, acc ->
      acc + count_in_direction(grid, word, {dx, dy})
    end)
  end

  defp count_in_direction(grid, word, {dx, dy}) do
    height = length(grid)
    width = length(List.first(grid))

    Enum.reduce(0..(width - 1), 0, fn x, acc_x ->
      acc_x + Enum.reduce(0..(height - 1), 0, fn y, acc_y ->
        acc_y + match_word_at(grid, word, x, y, {dx, dy})
      end)
    end)
  end

  defp match_word_at(grid, word, x, y, {dx, dy}) do
    word_length = String.length(word)

    Enum.reduce_while(0..(word_length - 1), "", fn i, acc ->
      new_x = x + i * dx
      new_y = y + i * dy

      if valid_position?(grid, new_x, new_y) do
        letter = Enum.at(Enum.at(grid, new_y), new_x)
        {:cont, acc <> letter}
      else
        {:halt, ""}
      end
    end)
    |> (fn result ->
          if result == word or result == String.reverse(word) do
            1
          else
            0
          end
        end).()
  end

  defp valid_position?(grid, x, y) do
    y >= 0 and y < length(grid) and
      x >= 0 and x < length(Enum.at(grid, y, []))
  end
end

{:ok, contents} = File.read("wordsearch.txt")
XmasCounter.count_xmas(contents, 140)
