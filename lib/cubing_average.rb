class CubingAverage
  include Comparable

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

  def <=>(other) # TODO this logic could be probably moved into a module and shared with Single, Average and Record
    if not self.dnf? and not other.dnf?
      self.time <=> other.time
    elsif self.dnf? and not other.dnf?
      1
    elsif other.dnf? and not self.dnf?
      -1
    else # both dnf
      0
    end
  end

  def best
    return @singles.first if solved_singles.empty?
    solved_singles.sort_by(&:time).first
  end

  def worst
    dnf = @singles.detect(&:dnf?)
    return dnf if dnf
    @singles.sort_by(&:time).last
  end

  def time
    @time ||=(
      return nil if @singles.empty?

      if @singles.size == 1
        return @singles.first.dnf? ? nil : @singles.first.time
      end

      dnfs = @singles.size - solved_singles.size

      return nil if dnfs > 1

      sorted_singles = solved_singles.sort_by(&:time)

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
