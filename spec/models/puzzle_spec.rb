require "spec_helper"

describe Puzzle do
  describe "scrambles" do
    it "delivers 25 move 3x3x3 scramble" do
      Puzzle.new(:name => "3x3x3", :attempt_count => 25).scramble.split(" ").should have(25).elements
    end
  end
end
