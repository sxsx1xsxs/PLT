{ open Parser }

rule tokenize = parse
    [' ', '\t', '\n'] { tokenize lexbuf }
  | '+'   {PLUS}
  | '-'   {MINUS}
  | '*'   {TIMES}
  | '/'   {DIVIDE}
  | '%'   {MOD}
  | '@'   {REGEX}
  | '=='  {EQ}
  | '!='  {NEQ}
  | '>>'  {RAPPEND}
  | '<<'  {LAPPEND}
  | '>'   {LR}
  | '<'   {RL}
  | 'and' {AND}
  | 'or'  {OR}
  | 'not' {NOT}
