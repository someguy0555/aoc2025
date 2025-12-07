
function main()
   local lines
   local splits = 0

   lines = get_lines()
   local ret = get_lines_and_splits(lines)
   lines = ret.lines
   splits = ret.splits

   -- for i, curr_line in ipairs(lines) do
   --     print(curr_line)
   -- end
   print(splits)
end

function get_lines()
    local lines = {}
    for line in io.lines() do
        table.insert(lines, line)
    end
    return lines
end

-- Source - https://stackoverflow.com/a
-- Posted by RBerteig, modified by community. See post 'Timeline' for change history
-- Retrieved 2025-12-07, License - CC BY-SA 3.0
function replace_char(pos, str, r)
    return str:sub(1, pos-1) .. r .. str:sub(pos+1)
end

function get_lines_and_splits(lines)
   local splits = 0
   for i, curr_line in ipairs(lines) do
       if i > 1 then
           local prev_line = lines[i-1]
           for j = 1, #curr_line do
               local char = curr_line:sub(j,j)
               local char_above = prev_line:sub(j,j)

               if char_above == '|' or char_above == 'S' then
                   if char == '.' then
                       curr_line = replace_char(j,curr_line,'|')
                   elseif char == '^' then
                       splits = splits + 1
                       if 1 < j then
                           curr_line = replace_char(j - 1,curr_line,'|')
                       end
                       if j < #curr_line then
                           curr_line = replace_char(j + 1,curr_line,'|')
                       end
                   end
               end
           end
           lines[i] = curr_line
       end
   end

   return { splits = splits, lines = lines}
end

main()
