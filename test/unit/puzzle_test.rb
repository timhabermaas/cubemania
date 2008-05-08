require File.dirname(__FILE__) + '/../test_helper'

class PuzzleTest < ActiveSupport::TestCase
  def test_square_one_slice_move
    Puzzle.publicize_methods do
      puzzle = Puzzle.new
      a = [30,30,30,60,30] + [60,60,60]
      b = [30,60,60,30] + [30,30,60,60]
      puzzle.do_slice a, b
      assert a == [30,60,60,30] + [60,60,60]
      assert b == [30,60,30,30,30] + [30,30,60,60]
      puzzle.do_slice a, b
      assert a == [30,30,30,60,30] + [60,60,60]
      assert b == [30,60,60,30] + [30,30,60,60]
      
      a = [60,60,60] + [60,30,30,60]
      b = [30,60,30,30,30] + [60,60,30,30]
      puzzle.do_slice a,b
      assert a == [30,30,30,60,30] + [60,30,30,60]
      assert b == [60,60,60] + [60,60,30,30]
    end
  end
  def test_square_one_layer_move
    Puzzle.publicize_methods do
      puzzle = Puzzle.new
      a = %w(a b c d)
      puzzle.do_move a, 0
      assert a == %w(a b c d)
      puzzle.do_move a, 1
      assert a == %w(b c d a)
      puzzle.do_move a, 2
      assert a == %w(d a b c)
      puzzle.do_move a, 5
      assert a == %w(a b c d)
      puzzle.do_move a, -1
      assert a == %w(d a b c)
      puzzle.do_move a, -4
      assert a == %w(d a b c)
    end
  end
  def test_square_one_check_moves
    Puzzle.publicize_methods do
      layer = [60,60,30,60,30,30,60,30]
      assert Puzzle.new.check_moves(layer) == [1,3,-3,-1]
      layer = [60,60,60,60,60,60]
      assert Puzzle.new.check_moves(layer) == [0,1,2,-3,-2,-1]
      layer = [30,60,30,30,60,60,30,60]
      assert Puzzle.new.check_moves(layer) == [1,2,3,-3,-2,-1]
      layer = [30,60,30,60,30,60,60,30]
      assert Puzzle.new.check_moves(layer) == [0,1,2,-4,-3,-2]
      layer = [60,60,130]
      assert Puzzle.new.check_moves(layer) == []
    end
  end
  def test_square_one
    Puzzle.publicize_methods do
      p = Puzzle.new
      layer1 = [30,60,30,60,30,60,30,60]
      layer2 = [30,60,30,60,30,60,30,60]
      100.times do
        up_move = p.check_moves(layer1).rand
        up_move = 0 if up_move.nil?
        down_move = p.check_moves(layer2).rand
        down_move = 0 if down_move.nil?
        p.do_move layer1, up_move
        p.do_move layer2, down_move
        p.do_slice layer1, layer2
        assert layer1.sum == 360
        assert layer2.sum == 360
      end
    end
  end
end
