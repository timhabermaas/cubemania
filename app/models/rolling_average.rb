class RollingAverage
  def initialize(size)
    @size = size
    @singles = []
    @dnfs = 0
    @sum = 0.0
  end

  def <<(single)
    @singles << single
    @dnfs += 1 if single.dnf?

    if @singles.size > @size
      removed_single = @singles.slice
      removed_time = removed_single.time
      @dnfs -= 1 if removed_single.dnf?
    else
      removed_single = nil
      removed_time = 0
    end

    @sum += single.time - removed_time
    # keep track of fastest and slowest
  end

  def average
    @dnfs > 1 ? nil : @sum / @singles.size
  end
end