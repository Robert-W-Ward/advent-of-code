# 0 = means no free space
# each even index has an "ID" which is just the order in which it appears i.e. 12345 would have
# {0,1} would be a 1 block file with index 0 and ID = 0
# {1,2} would be a 2 block free space with index 1 and no id
# {2,3} would be a 3 block file with index 2 and ID = 1
# {3,4} would be a 4 block free space with index 3 and no id
# {4,5} would be a 5 block file with index 4 and ID = 2

# ID = number of files starting at zero
# idx = even/file odd/empty
# list[idx] = character that should be repeated ID times
{:ok, contents} = File.read("disk.txt")
disk = String.graphemes(contents)
disk1 = String.graphemes("2333133121414131402")

expand = fn list ->
  list
  |> Enum.with_index()
  |> Enum.reduce({[], 0}, fn {char, idx}, {acc, even_count} ->
    # Convert ~c"1" into an integer 1
    num = String.to_integer(char)

    cond do
      rem(idx, 2) == 0 ->
        # Even index: print the current even_count repeated num times
        updated_acc = acc ++ List.duplicate(Integer.to_charlist(even_count), num)
        {updated_acc, even_count + 1}

      true ->
        # Odd index: print "." repeated num times
        updated_acc = acc ++ List.duplicate(~c".", num)
        {updated_acc, even_count}
    end
  end)
  # Extract the final list from the accumulator
  |> elem(0)
end

compact = fn ^disk, expanded_disk ->
  move_file_block = fn disk ->
    leftmost_empty = Enum.find_index(disk, &(&1 == ~c"."))

    # If there's no empty space, we’re done
    if is_nil(leftmost_empty) do
      {:done, disk}
    else
      # Find the rightmost file block to the right of this empty space
      # Files are not '.', so filter them out:
      rightmost_file =
        disk
        |> Enum.with_index()
        |> Enum.reject(fn {c, _i} -> c == ~c"." end)
        |> Enum.filter(fn {_c, i} -> i > leftmost_empty end)
        |> List.last()

      # If no file block is found to the right, we can’t move anymore
      if is_nil(rightmost_file) do
        {:done, disk}
      else
        {file_char, file_idx} = rightmost_file

        # Swap the file block with the empty space
        disk =
          disk
          |> List.replace_at(leftmost_empty, file_char)
          |> List.replace_at(file_idx, ~c".")

        {:continue, disk}
      end
    end
  end

  # Repeatedly move one file block at a time until no more moves are possible
  Enum.reduce_while(Stream.cycle([:tick]), expanded_disk, fn _, acc ->
    case move_file_block.(acc) do
      {:done, final_disk} -> {:halt, final_disk}
      {:continue, new_disk} -> {:cont, new_disk}
    end
  end)
end

defrag = fn unexpanded_disk, expanded_disk, file_ids, free_blocks ->
  Enum.reduce(file_ids, {expanded_disk, free_blocks}, fn file_id_map,
                                                         {disk_acc, free_blocks_acc} ->
    {char, _abs_idx, file_id} = file_id_map
    file_size = String.to_integer(char)

    # Find all original positions of this file ID
    original_positions =
      Enum.filter(Enum.with_index(disk_acc), fn {block, _idx} ->
        block == Integer.to_charlist(file_id)
      end)
      |> Enum.map(fn {_block, idx} -> idx end)

    # If file doesn't exist on disk (edge case), skip
    if original_positions == [] do
      {disk_acc, free_blocks_acc}
    else
      file_min_idx = Enum.min(original_positions)

      # Find a suitable free block: must be to the left of file_min_idx and large enough
      suitable_block =
        free_blocks_acc
        |> Enum.filter(fn {fs, fe} ->
          fs < file_min_idx and fe - fs + 1 >= file_size
        end)
        |> Enum.sort_by(fn {fs, _fe} -> fs end)
        |> List.first()

      # If no suitable block is found, skip moving the file
      if is_nil(suitable_block) do
        {disk_acc, free_blocks_acc}
      else
        {start_idx, end_idx} = suitable_block
        free_space_size = abs(end_idx - start_idx) + 1

        if file_size <= free_space_size do
          # Remove the chosen block from free_blocks_acc
          tail = free_blocks_acc -- [suitable_block]

          # Step 1: Clear old file positions (replace with ".")
          cleared_disk =
            Enum.reduce(original_positions, disk_acc, fn idx, acc ->
              List.replace_at(acc, idx, ~c".")
            end)

          # Step 2: Move the file into the new free block
          updated_disk =
            Enum.reduce(0..(file_size - 1), cleared_disk, fn offset, acc ->
              List.replace_at(acc, start_idx + offset, Integer.to_charlist(file_id))
            end)

          # Step 3: Update the free blocks list accordingly
          new_free_blocks =
            cond do
              file_size == free_space_size ->
                tail

              file_size < free_space_size ->
                [{start_idx + file_size, end_idx} | tail]
            end

          {updated_disk, new_free_blocks}
        else
          {disk_acc, free_blocks_acc}
        end
      end
    end
  end)
end

checksum = fn disk ->
  disk
  |> Enum.with_index()
  |> Enum.reduce(0, fn {char, idx}, acc ->
    if char == ~c"." do
      acc
    else
      # Convert file ID character back to an integer
      file_id =
        case char do
          c when is_integer(c) -> String.to_integer(List.to_string([c]))
          _ -> String.to_integer(to_string(char))
        end

      acc + idx * file_id
    end
  end)
end

# expanded_disk = expand.(disk)
# compacted_disk = compact.(disk, expanded_disk)

# checksum1 = checksum.(compacted_disk)
# IO.puts("Part1 #{checksum1}")

free_blocks = fn disk ->
  Enum.with_index(disk)
  |> Enum.reduce({[], nil, nil}, fn
    {~c".", idx}, {ranges, start, _end} ->
      if start == nil do
        {ranges, idx, idx}
      else
        {ranges, start, idx}
      end

    {_, _idx}, {ranges, start, end_idx} when start != nil ->
      {[{start, end_idx} | ranges], nil, nil}

    {_char, _idx}, acc ->
      acc
  end)
  |> (fn {ranges, start, end_idx} ->
        if start != nil do
          [{start, end_idx} | ranges]
        else
          ranges
        end
      end).()
  |> Enum.reverse()
  |> Enum.to_list()
end

file_ids = fn disk ->
  disk
  |> Enum.with_index()
  |> Enum.take_every(2)
  |> Enum.with_index()
  |> Enum.sort(fn {_a, b}, {_b, d} -> d <= b end)
  |> Enum.map(fn tuple ->
    {inner, file_id} = tuple
    {char, absolute_idx} = inner
    {char, absolute_idx, file_id}
  end)
end

expanded_disk = expand.(disk)
# IO.puts("Expanded Disk: #{expanded_disk}\n")
free_blocks = free_blocks.(expanded_disk)
# IO.puts("Free blocks: #{inspect(free_blocks)}\n")
file_ids = file_ids.(disk)
# IO.puts("File IDs: #{inspect(file_ids)}\n")

defragged = defrag.(disk, expanded_disk, file_ids, free_blocks)
defragged |> elem(0) |> checksum.() |> IO.inspect()
IO.puts("Part2 #{defragged}")
