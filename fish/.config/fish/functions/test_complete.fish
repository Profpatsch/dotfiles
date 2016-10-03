function test_complete --argument test_command
	complete -c "$test_command" -e
	complete -c "$test_command" -xa "(eval $argv[2..-1])"
end
