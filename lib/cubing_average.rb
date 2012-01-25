class CubingAverage
  include Comparable

  attr_reader :singles

  def initialize(singles = [])
    @singles = singles.dup
  end

  def <<(single)
    @singles << single
    @time = nil
  end

  def <=>(other)
    if not self.dnf? and not other.dnf?
      self.time <=> other.time
    elsif self.dnf? and not other.dnf?
      1
    elsif other.dnf? and not self.dnf?
      -1
    else # wtf?
    end
  end

  def time
    @time ||=(
      return nil if singles.empty?

      if @singles.size == 1
        return @singles.first.dnf? ? nil : @singles.first.time
      end

      cleaned_up_singles = @singles.reject(&:dnf?)
      dnfs = @singles.size - cleaned_up_singles.size

      return nil if dnfs > 1

      cleaned_up_singles = cleaned_up_singles.sort_by(&:time)

      if dnfs == 0
        cleaned_up_singles[1..-2].map(&:time).inject(:+) / (cleaned_up_singles.size - 2)
      else
        cleaned_up_singles[1..-1].map(&:time).inject(:+) / (cleaned_up_singles.size - 1)
      end)
  end

  def dnf?
    time.nil?
  end
end
