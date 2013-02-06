class UpdateRecentRecords
  def self.for(user, puzzle)
    RecordType.all.map do |type|
      for_amount(user, puzzle, type)
    end.any? { |e| e }
  end

  def self.for_amount(user, puzzle, type)
    singles = Single.for_user_and_puzzle(user, puzzle).recent(type.count)
    return if singles.size < type.count

    average = CubingAverage.new(singles)
    Record.update_with!(user, puzzle, type.count, average.time, singles) unless average.dnf?
  end
end
