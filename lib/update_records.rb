class UpdateRecords
  def self.for(user, puzzle)
    single(user, puzzle)
    for_amount(user, puzzle, 5)
    for_amount(user, puzzle, 12)
  end

  def self.single(user, puzzle)
    best = user.singles.best(puzzle)
    if best
      Record.update_with! user, puzzle, 1, best.time, [best], true
    else
      Record.remove! user, puzzle, 1
    end
  end

  def self.for_amount(user, puzzle, amount)
    best_average = user.best_average(puzzle, amount)
    if best_average.dnf?
      Record.remove! user, puzzle, amount
    else
      Record.update_with! user, puzzle, amount, best_average.time, best_average.singles, true
    end
  end
end
