open Parse;;

let in_rule = 
    let read_file =  Lexing.from_channel (open_in file) in 
    	let program = Parser.prog Lexer.convert read_file;;

let query prog str = 
	let read_string = Lexing.from_string str in 
		let goal = Parser.goal Lexer.convert read_string in
			A6.solve program program goal [] [] [] ;;

let rec main program = 
	