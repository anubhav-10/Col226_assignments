open Parse;;

let in_rule = 
    let read_file =  Lexing.from_channel (open_in file) in 
    	let program = Parser.prog Lexer.convert read_file;;

let query prog str = 
	let 