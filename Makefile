NUM ?= 1

sml:
# Description: run sml REPL with rlwrap
# make sml
	rlwrap sml

week:
# Description: load homework NUM=n in a sml REPL (default NUM=1)
# ex: make week NUM=1
	rlwrap sml ./homework$(NUM)/hw$(NUM).sml

test:
# Description: load test NUM=n in a sml REPL
# ex: make test NUM=1
	rlwrap sml ./homework$(NUM)/hw$(NUM)_test.sml

run:
# Description: execute sml REPL using a given file
# ex: make run file=filename.sml
	rlwrap sml $(file)

ruby-game-original:
# Description: execute ruby tetris game
# ex: make ruby-game-original
	ruby ./homework6/hw6runner.rb original

ruby-game-enhanced:
# Description: execute ruby tetris game with enhancements
# ex: make ruby-game-enhanced
	ruby ./homework6/hw6runner.rb enhanced
