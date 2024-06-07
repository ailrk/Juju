set pagination off
set logging file debug.gdb.out
set logging overwrite on
set logging enabled on

define show_context
    info locals
    printf "q: "
    print *q@10
    printf "a:"
    print *a@10
    printf "k: %d\n", k
    printf "n: %d\n", n
    continue
end

break  50
commands
    show_context
end

break 52
commands
    show_context
    printf "i: %d\n", i
end

break 61
commands
    show_context
    printf "i: %d\n", i
end

run

set logging enabled off
quit
