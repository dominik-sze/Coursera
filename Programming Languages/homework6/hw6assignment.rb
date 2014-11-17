# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new(Cheat_Piece, board)
  end

  Cheat_Piece = [[[0,0]]]

  All_My_Pieces = All_Pieces +
		  [rotations([[0, 0], [0, 1], [1, 0]]), # small L
	       	  rotations([[0, 0], [0, 1], [1, 0], [1, 1], [-1, 0]]), # square with extra element
  		  [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]], # very long (only needs two)
                  [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]]]
end

class MyBoard < Board

  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @game  = game
    @score = 0
    @delay = 500
    @cheat = false
  end

  def rotate_180_degrees
    rotate_clockwise
    rotate_clockwise
  end

  def cheating
    if @score >= 100 and !@cheat
      @cheat = true
      @score -= 100
    end
  end

  def next_piece
    if @cheat
      @current_block = MyPiece.cheat_piece(self)
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size-1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end

class MyTetris < Tetris

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180_degrees})
    @root.bind('c', proc {@board.cheating})
  end

end


