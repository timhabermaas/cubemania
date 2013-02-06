class Mean
  def initialize(solves)
    @solves = solves
  end

  def result
    return nil if @solves.any? { |s| s.dnf? }

    @solves.inject(0.0) { |sum, s| sum + s.time } / @solves.size
  end
end
