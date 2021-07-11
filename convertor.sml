exception emptyInputFile
exception notEnclosedByQuotes
exception unescapedQuotes
exception unevenFields of string
exception missing_LF_last_record
exception delim_at_last_field

fun cvt_helper(infilename : string, delim1 : char, outfilename : string, delim2 : char) = 
	let 
		val fin = TextIO.openIn infilename
		val fout = TextIO.openOut outfilename
		val d = TextIO.input1 fin : char option;
		val line_count = ref(1)
		val quote = ref(false)
		val field_count = ref(0)
		val this_field_count = ref(0)
		val this_field = ref("")

		fun helper(copt : char option) = 
			case copt of 
				NONE => (
					if not(!this_field = "") then 
						raise missing_LF_last_record
					else 
						TextIO.closeIn fin; 
						TextIO.closeOut fout
					) 
				| SOME(c) => (

					if c = #"\"" then 
						if !quote then(
							quote := false;
							this_field := !this_field ^ "\"" )
						else(
							if !this_field = "" then(
								quote := true;
								this_field := !this_field ^ "\""
								)
							else(
								if not(String.sub(!this_field, String.size(!this_field)-1) = #"\"") then
									raise unescapedQuotes
								else if not( String.sub(!this_field,1) = #"\"") then 
									raise notEnclosedByQuotes
								else(
									quote := true;
									this_field := !this_field ^ "\"")
								)
							)

					else if c = delim1 then
						if !quote then(
							this_field := !this_field ^ Char.toString(c) )
						else(
							TextIO.output(fout, !this_field);
							TextIO.output1(fout, delim2);
							this_field := "";
							this_field_count := !this_field_count + 1 )

					else if c = #"\n" then
						if !quote then(
							if not(String.size(!this_field) = 0 orelse String.sub(!this_field, 0) = #"\"") then 
								raise notEnclosedByQuotes
							else
								this_field := !this_field ^ "\n" )
						else if (!this_field = "") then (
							raise delim_at_last_field
							)
						else(
							if !field_count = 0 then (
								line_count := !line_count + 1;
								field_count := !this_field_count;
								this_field_count := 0;
								TextIO.output(fout, !this_field);
								TextIO.output1(fout, c);
								this_field := "" )
							else if not(!field_count = !this_field_count) then(
								raise unevenFields("Expected: " ^ Int.toString(!field_count + 1) ^" fields, Present: " ^ Int.toString(!this_field_count + 1) ^ " fields on Line " ^ Int.toString(!line_count) ^ "\n") )
							else(
								line_count := !line_count + 1;
								field_count := !this_field_count;
								this_field_count := 0;
								TextIO.output(fout, !this_field);
								TextIO.output1(fout, c);
								this_field := "")
							)

					else if c = delim2 then
						if String.sub(!this_field, 1) = #"\"" then
							this_field := !this_field ^ Char.toString(c)
						else
							this_field :=  Char.toString(#"\"") ^ !this_field ^ Char.toString(c)

					else if c = #"," then(
						if not(String.size(!this_field) = 0 orelse String.sub(!this_field, 0) = #"\"") then 
							raise notEnclosedByQuotes
						else 
							this_field := !this_field ^ Char.toString(c)
						)
					else
						this_field := !this_field ^ Char.toString(c);

					helper(TextIO.input1 fin)
					)
	in  
		if (d = NONE) then(
				raise emptyInputFile )
		else(
			helper(d) )
	end	

fun convertDelimiters(infilename : string, delim1 : char, outfilename : string, delim2 : char) = cvt_helper(infilename, delim1, outfilename, delim2) handle 
	unevenFields(x) => ( print("Uneven field exception.\n"); print(x) );

fun csv2tsv(infilename : string, outfilename : string) =
	convertDelimiters(infilename , #"," , outfilename, #"\t")

fun tsv2csv(infilename : string, outfilename : string) =
	convertDelimiters(infilename , #"\t" , outfilename, #",")
