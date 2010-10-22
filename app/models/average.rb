class Average
  attr_accessor :singles

  def initialize(singles)
    @singles = singles
  end

  def average
    non_dnf_singles = @singles.reject { |s| s.dnf? }

    return if non_dnf_singles.size < @singles.size - 1

    non_dnf_singles.sort_by { |s| s.time }.slice(0)
    non_dnf_singles.inject(0) { |sum, s| sum + s.time } / non_dnf_singles.size.to_f
  end
end