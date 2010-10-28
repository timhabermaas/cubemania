class RollingAverage
  def initialize(size, singles = [])
    @size = size
    @singles = []
    @dnfs = 0
    @sum = 0.0
    singles.each do |single|
      self.<<(single)
    end
  end

  def <<(single)
    @singles << single # single.time ??

    if @best == nil || single.time < @best.time
      @best = single
    end
    if @worst == nil || single.time > @worst.time || single.dnf?
      @worst = single
    end

    @dnfs += 1 if single.dnf?

    if @singles.size > @size
      removed_single = @singles.shift
      removed_time = removed_single.time
      @dnfs -= 1 if removed_single.dnf?
    else
      removed_single = nil
      removed_time = 0
    end

    @sum += single.time - removed_time
    # keep track of fastest and slowest
  end

  # consider caching the result (clear cache in <<)
  def average
    return nil if @dnfs > 1 || @singles.size < @size || (@singles.size == 1 and @dnfs == 1)
    times = @singles.reject { |s| s.dnf? }.collect { |s| s.time }
    best = times.min
    worst = @singles.select { |s| s.dnf? }.first.try(:time) || times.max

    (@sum - best - worst) / (@singles.size - 2)
  end
end