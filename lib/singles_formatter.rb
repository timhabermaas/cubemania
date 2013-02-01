class SinglesFormatter
  def initialize(singles)
    @singles = singles
  end

  def as_text
    if @singles.size == 1
      @singles.first.human_time(:unit => false)
    else
      @singles.map do |s|
        formated_time = s.dnf? ? "DNF" : s.human_time(:unit => false)
        if [best, worst].include? s
          "(" + formated_time + ")"
        else
          formated_time
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
end
