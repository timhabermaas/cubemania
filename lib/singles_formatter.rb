class SinglesFormatter
  def initialize(singles)
    @singles = singles
  end

  def as_text
    if @singles.size == 1
      formatted_time(@singles.first)
    else
      @singles.map do |s|
        if [best, worst].include? s
          "(" + formatted_time(s) + ")"
        else
          formatted_time(s)
        end
      end.join " "
    end
  end

private
  def best
    @min ||= @singles.min
  end

  def worst
    @max ||= @singles.max
  end

  def formatted_time(single)
    return "DNF" if single.dnf?
    single.human_time(:unit => false) + (single.plus2? ? "+" : "")
  end
end
