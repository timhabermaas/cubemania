class UpdateRecentRecords
  def self.for(user, puzzle)
    [1, 5, 12].map do |amount|
      for_amount(user, puzzle, amount)
    end.any? { |e| e }
  end

  def self.for_amount(user, puzzle, amount)
    singles = Single.for_user_and_puzzle(user, puzzle).recent(amount)
    return if singles.size < amount

    average = CubingAverage.new(singles)
    Record.update_with!(user, puzzle, amount, average.time, singles) unless average.dnf?
  end
end
