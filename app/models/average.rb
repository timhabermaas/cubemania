class Average
  attr_accessor :singles

  def initialize(singles)
    @singles = singles
  end

  # Returns the average time for given singles. Returns nil if the average is a DNF.
  def time
    non_dnf_singles = @singles.reject { |s| s.dnf? }

    return if non_dnf_singles.size < @singles.size - 1

    non_dnf_singles.sort_by! { |s| s.time }

    if non_dnf_singles.size == @singles.size # there's no dnf
      non_dnf_singles.pop
    end
    non_dnf_singles.slice!(0)

    non_dnf_singles.inject(0) { |sum, s| sum + s.time } / non_dnf_singles.size.to_f
  end
end