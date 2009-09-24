require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

describe Puzzle, "scrambling length" do
  
  it "should give a 25 move scramble for a 3x3x3" do
    puzzle = Factory.create(:puzzle)
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
    puzzle = Factory.create(:puzzle)
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