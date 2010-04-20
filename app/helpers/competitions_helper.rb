module CompetitionsHelper
  def repeat?(*repeats)
    repeats.include? self.repeat.to_sym
  end

  def repeat
    params[:repeat] || (@competition.repeat unless @competition.nil?)
  end

  def nominalize_repeat
    repeat == 'daily' ? 'day' : repeat[0..-3]
  end
end