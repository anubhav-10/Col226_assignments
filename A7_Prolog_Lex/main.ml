open Parser;;

let in_rule file = 
    let read_file =  Lexing.from_channel (open_in file) in 
    	let program = Parser.prog Lexer.convert read_file in
                program
;;

let query program str = 
	let read_string = Lexing.from_string str in 
		let goal = Parser.goal Lexer.convert read_string in
			A6.solve program program goal [] [] [] ;;

let rec main = 
	Printf.printf "?- ";
	let input = (try read_line() 
				with End_of_file -> Printf.printf "\nExiting"; exit 0) in

	if (input = "exit.") then exit 0
	else 
		let new_prog = in_rule (Sys.argv.(1)) in
			query new_prog input;;

if !Sys.interactive then () else main;;
