function h --description head --argument number_lines with number of lines
	head -n $number_lines $argv[2..-1]
end
