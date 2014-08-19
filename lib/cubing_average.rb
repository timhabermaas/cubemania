require "comparable_solve"

class CubingAverage
  include ComparableSolve

  attr_reader :singles

  def initialize(singles = [], time = nil)
    @singles = singles.dup
    @time = time
  end

  def <<(single)
    @singles << single
    @time = nil
    @solved_singles = nil
  end

  def best
    return @singles.first if solved_singles.empty?
    solved_singles.min
  end

  def worst
    dnf = @singles.detect(&:dnf?)
    return dnf if dnf
    @singles.max
  end

  def time
    @time ||=(
      return nil if @singles.empty?

      if @singles.size == 1
        return @singles.first.dnf? ? nil : @singles.first.time
      end

      dnfs = @singles.size - solved_singles.size

      return nil if dnfs > 1

      sorted_singles = solved_singles.sort

      if dnfs == 0
        sorted_singles[1..-2].map(&:time).inject(:+) / (sorted_singles.size - 2).to_f
      else
        sorted_singles[1..-1].map(&:time).inject(:+) / (sorted_singles.size - 1).to_f
      end)
  end

  def dnf?
    time.nil?
  end

  private
  def solved_singles
    @solved_singles ||= singles.reject(&:dnf?)
  end
end
