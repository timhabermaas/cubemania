require "mean"

class Average
  def initialize(solves)
    @solves = solves
  end

  def result
    return nil if @solves.count { |s| s.dnf? } > 1

    best = @solves.min
    worst = @solves.max
    relevant_singles = @solves - [best] - [worst]
    Mean.new(relevant_singles).result
  end
end
