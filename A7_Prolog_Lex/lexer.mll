{
    open Parser
    exception Eof
}

let vars = (['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*)
let const = (['a'-'z']['a'-'z''A'-'Z''0'-'9''_']*)
let comma = ','
let open_par = '('
let close_par = ')'
let open_squ = '['
let close_squ = ']'
let space = [' ''\t']+
let rule_sep = ":-"
let semi_colon = ';'
let eol = '\n'
let dot = '.'

rule convert = parse
    | vars as s     {VARS(s)}
    | const as s    {CONST(s)}
    | comma         {COMMA}
    | open_par      {OPEN_PAR}
    | close_par     {CLOSE_PAR}
    | space         {convert lexbuf}
    | rule_sep      {RULE_SEP}
    | semi_colon    {SEMI_COLON}
    | eol           {convert lexbuf}
    | eof           {EOF}
    | dot           {END} 
