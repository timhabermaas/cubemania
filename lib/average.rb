class Average
  def initialize(solves)
    @solves = solves
  end

  def result
    return nil if @solves.count { |s| s.dnf? } > 1

    best = @solves.min
    worst = @solves.max
    relevant_singles = @solves - [best] - [worst]
    relevant_singles.inject(0) { |sum, s| sum + s.time } / relevant_singles.size.to_f
  end
end
