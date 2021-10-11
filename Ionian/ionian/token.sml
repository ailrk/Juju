structure Token : IONIAN_TOKEN =
struct
  type pos = int
  type token = string
  ASSIGN (i, j) = "ASSIGN " ^ Int.toString(i)
  IF (i, j) = "IF " ^ Int.toString(i)
  THEN (i, j) = "THEN " ^ Int.toString(i)
  ELSE (i, j) = "ELSE " ^ Int.toString(i)
  WHILE (i, j) = "WHILE " ^ Int.toString(i)
  SKIP (i, j) = "SKIP " ^ Int.toString(i)
  SEMICOLON (i, j) = "SEMICOLON " ^ Int.toString(i)
  ADD (i, j) = "ADD " ^ Int.toString(i)
  SUB (i, j) = "SUB " ^ Int.toString(i)
  MUL (i, j) = "MUL " ^ Int.toString(i)
  OR (i, j) = "OR " ^ Int.toString(i)
  EQ (i, j) = "EQ " ^ Int.toString(i)
  LE (i, j) = "LE " ^ Int.toString(i)

  INT (c, i, j) = "INT(" ^ Int.toString(c) ^ ") " ^ Int.toString(i)
  ID (s, i, j) = "ID(" ^ Int.toString(s) ^ ") " ^ Int.toString(i)
  EOF (i, j) = "EOF " ^ Int.toString(i)
end
