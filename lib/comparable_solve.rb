module ComparableSolve
  include Comparable

  def <=>(other)
    return 0 if self.dnf? and other.dnf?
    return -1 if other.dnf?
    return 1 if self.dnf?

    self.time <=> other.time
  end
end
