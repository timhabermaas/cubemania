require "comparable_solve"

class Solve < Struct.new(:time, :dnf)
  include ComparableSolve # including <=> into ActiveRecord subclass messes with default equality operator -.-

  def dnf?
    !!dnf
  end
end

describe ComparableSolve do
  context "solved vs solved" do
    it "sorts by time" do
      expect(Solve.new(10, false)).to be < Solve.new(12, false)
      expect(Solve.new(13, false)).to be > Solve.new(9, false)
      expect(Solve.new(13, false)).to be == Solve.new(13, false)
    end
  end

  context "solved vs not solved" do
    it "lets the DNF always be worse" do
      expect(Solve.new(10, true)).to be > Solve.new(12, false)
      expect(Solve.new(13, false)).to be < Solve.new(13, true)
      expect(Solve.new(14, false)).to be < Solve.new(13, true)
    end
  end

  context "not solved vs not solved" do
    it "makes them equal" do
      expect(Solve.new(10, true)).to be == Solve.new(12, true)
    end
  end
end
