# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [
    [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]], # extra long (only needs two)
     [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],
     rotations([[0, 0], [0, 1], [1, 0]]), # short L
     rotations([[1, -1], [1, 0], [0, -1], [0, 0], [0, 1]]) # new block
  ]

  # class method to choose the next piece
  def self.next_piece(board)
    new((All_My_Pieces + All_Pieces).sample, board)
  end

  def self.cheat_next_piece(board)
    single_block = [[[0, 0]]]
    new(single_block, board)
  end
end

class MyBoard < Board
  def initialize(game)
    super
    @current_block = MyPiece.next_piece(self)
    @cheat_status = false
  end

  # rotates the current piece 180 degrees
  def rotate_180_degrees
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, -2)
    end

    draw
  end

  def drop_cheat_piece
    return if score < 100 || @cheat_status
    
    @score -= 100
    @cheat_status = true
  end

  # gets the next piece. If cheat_status is enabled, returns custom piece
  # and reset the cheat status
  def next_piece
    @current_block =  @cheat_status ? MyPiece.cheat_next_piece(self) : MyPiece.next_piece(self)
    @current_pos = nil
    @cheat_status = false
  end

  # custom method that considers block sizes with less than 4 items (cheat_piece and short L)
  def store_current
    locations = @current_block.current_rotation
    block_size = locations.size
    displacement = @current_block.position

    (0..(block_size - 1)).each do |index| 
      current = locations[index];
      @grid[current[1] + displacement[1]][current[0] + displacement[0]] = @current_pos[index]
    end

    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # creates a canvas and the enhanced board
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings  
    super
    @root.bind('u' , proc {@board.rotate_180_degrees})
    @root.bind('c' , proc {@board.drop_cheat_piece})
  end
end

