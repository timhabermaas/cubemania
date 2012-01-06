require "spec_helper"

describe Puzzle, "scrambling length" do
  it "should give a 25 move scramble for a 3x3x3" do
    puzzle = Factory.create(:puzzle, :name => '3x3x3', :scramble_length => 25)
    puzzle.scramble.split(/ /).size.should eql(25)
  end

  it "should give a 40 move scramble for a 4x4x4" do
    puzzle = Factory.create(:puzzle, :name => '4x4x4', :scramble_length => 40)
    puzzle.scramble.split(/ /).size.should eql(40)
  end

  it "should give a 50 move scramble for a 4x4x4" do
    puzzle = Factory.create(:puzzle, :name => '5x5x5', :scramble_length => 50)
    puzzle.scramble.split(/ /).size.should eql(50)
  end

  it "should give a 25 move scramble for a 2x2x2" do
    puzzle = Factory.create(:puzzle, :name => '2x2x2', :scramble_length => 25)
    puzzle.scramble.split(/ /).size.should eql(25)
  end
end

describe Puzzle, "proper scrambling turns" do
  before(:each) do
    @available_moves = %w(R R' R2 L L' L2 F F' F2 B B' B2 D D' D2 U U' U2)
    @available_moves_big = %w(R R' R2 Rw Rw2 Rw' L L' L2 Lw Lw' Lw2 F F' F2 Fw Fw' Fw2 B B' B2 Bw Bw' Bw2 D D' D2 Dw Dw' Dw2 U U' U2 Uw Uw' Uw2)
  end

  it "should give only the available moves for 3x3x3" do
    puzzle = Factory.create(:puzzle, :name => '3x3x3')
    puzzle.scramble.split(/ /).each do |move|
      @available_moves.should include(move)
    end
  end

  it "should give only the available moves for 2x2x2" do
    puzzle = Factory.create(:puzzle, :name => '2x2x2')
    puzzle.scramble.split(/ /).each do |move|
      @available_moves.should include(move)
    end
  end

  it "should give only the available moves for 4x4x4" do
    puzzle = Factory.create(:puzzle, :name => '4x4x4')
    puzzle.scramble.split(/ /).each do |move|
      @available_moves_big.should include(move)
    end
  end

  it "should give only the available moves for 5x5x5" do
    puzzle = Factory.create(:puzzle, :name => '5x5x5')
    puzzle.scramble.split(/ /).each do |move|
      @available_moves_big.should include(move)
    end
  end
end

describe Puzzle, "square-1" do
  it "makes proper slice moves" do
    Puzzle.publicize_methods do
      a = [30,30,30,60,30] + [60,60,60]
      b = [30,60,60,30] + [30,30,60,60]
      subject.do_slice a, b
      assert a == [30,60,60,30] + [60,60,60]
      assert b == [30,60,30,30,30] + [30,30,60,60]
      subject.do_slice a, b
      assert a == [30,30,30,60,30] + [60,60,60]
      assert b == [30,60,60,30] + [30,30,60,60]

      a = [60,60,60] + [60,30,30,60]
      b = [30,60,30,30,30] + [60,60,30,30]
      subject.do_slice a,b
      assert a == [30,30,30,60,30] + [60,30,30,60]
      assert b == [60,60,60] + [60,60,30,30]
    end
  end

  it "can inner moves" do
    Puzzle.publicize_methods do
      a = %w(a b c d)
      subject.do_move a, 0
      assert a == %w(a b c d)
      subject.do_move a, 1
      assert a == %w(b c d a)
      subject.do_move a, 2
      assert a == %w(d a b c)
      subject.do_move a, 5
      assert a == %w(a b c d)
      subject.do_move a, 1
      assert a == %w(b c d a)
      subject.do_move a, 8
      assert a == %w(b c d a)
    end
  end

  it "knows about possible moves" do
    Puzzle.publicize_methods do
      layer = [60,60,30,60,30,30,60,30]
      assert subject.possible_moves(layer) == [1,3,5,7]
      layer = [60,60,60,60,60,60]
      assert subject.possible_moves(layer) == [0,1,2,3,4,5]
      layer = [30,60,30,30,60,60,30,60]
      assert subject.possible_moves(layer) == [1,2,3,5,6,7]
      layer = [30,60,30,60,30,60,60,30]
      assert subject.possible_moves(layer) == [0,1,2,4,5,6]
      layer = [60,60,130]
      assert subject.possible_moves(layer) == []
    end
  end

  it "what?" do
    Puzzle.publicize_methods do
      layer1 = [30,60,30,60,30,60,30,60]
      layer2 = [30,60,30,60,30,60,30,60]
      100.times do
        up_move = subject.possible_moves(layer1).rand
        up_move = 0 if up_move.nil?
        down_move = subject.possible_moves(layer2).rand
        down_move = 0 if down_move.nil?
        subject.do_move layer1, up_move
        subject.do_move layer2, down_move
        subject.do_slice layer1, layer2
        assert layer1.sum == 360
        assert layer2.sum == 360
      end
    end
  end

  it "translate moves into understandable notation" do
    Puzzle.publicize_methods do
      layer = [60,60,60,30,60,60,30]
      assert subject.humanize_sq_one_move(layer, 0) == 0
      assert subject.humanize_sq_one_move(layer, 1) == 2
      assert subject.humanize_sq_one_move(layer, 2) == 4
      assert subject.humanize_sq_one_move(layer, 3) == 6
      assert subject.humanize_sq_one_move(layer, 4) == -5
      assert subject.humanize_sq_one_move(layer, 5) == -3
      assert subject.humanize_sq_one_move(layer, 6) == -1
    end
  end
end
