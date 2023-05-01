# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  ALL_MY_PIECES = [
    [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]], # extra long (only needs two)
     [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],
     rotations([[0, 0], [0, 0], [0, 1], [1, 1]]), # L
     rotations([[0, -1], [0, 0], [0, 1], [1, 0], [1, -1]]) # new block
  ]

  # class method to choose the next piece
  def self.next_piece (board)
    new(ALL_MY_PIECES.sample, board)
  end
end

class MyBoard < Board

  def initialize(game)
    super
    @current_block = MyPiece.next_piece(self)
  end

  # rotates the current piece 180 degrees
  def rotate_180_degrees
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, -2)
    end
    draw
  end

  # gets the next piece
  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
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
  end
end

