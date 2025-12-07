
function main()
    local lines
    local splits = 0
 
    lines = get_lines()
 
    local ret = get_lines_and_splits(lines)
    lines = ret.lines
    splits = ret.splits
 
    -- for i, curr_line in ipairs(lines) do
    --     for j, char in ipairs(curr_line) do
    --         io.write(char)
    --     end
    --     print("")
    -- end

    local line = lines[#lines]
    local timelines = 0
    for i, c in ipairs(line) do
        if type(c) == "number" then
            -- print(c)
            timelines = timelines + c
        end
    end
    print(splits)
    print(timelines)
end

function get_lines()
    local lines = {}
    for line in io.lines() do
        if type(line) == "string" then
            table.insert(lines, str_to_table(line))
        end
    end
    return lines
end

function str_to_table(str)
    assert(type(str) == "string")
    t = {}
    for i = 1, #str do
        t[i] = str:sub(i, i)
    end
    return t
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

            for j, char in ipairs(curr_line) do
                local char_above = prev_line[j]
 
                if type(char_above) == "string" and char_above == 'S' then
                    if char == '.' then
                        curr_line[j] = 1
                    end
                elseif type(char_above) == "number" then
                    if char == '.' then
                        curr_line[j] = char_above
                    elseif type(char) == "number" then
                        curr_line[j] = curr_line[j] + char_above
                    elseif char == '^' then
                        splits = splits + 1
                        if 1 < j then
                            if type(curr_line[j - 1]) == "number" then
                                curr_line[j - 1] = curr_line[j - 1] + char_above
                            else
                                curr_line[j - 1] = char_above
                            end
                        end
                        if j < #curr_line then
                            if type(curr_line[j + 1]) == "number" then
                                curr_line[j + 1] = curr_line[j + 1] + char_above
                            else
                                curr_line[j + 1] = char_above
                            end
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
