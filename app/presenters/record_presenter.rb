class RecordPresenter
  attr_reader :record
  delegate :human_time, :to => :record

  def initialize(record)
    @record = record
  end

  def record_type
    amount = @record.amount
    case amount
    when 1
      "single"
    else
      "average of #{amount}"
    end
  end

  def flash_message
    "You have a new #{record_type.downcase} record: <strong>#{@record.human_time}</strong>!"
  end

  def full_puzzle_name
    kind = @record.puzzle.kind.name == "speed" ? "" : @record.puzzle.kind.short_name.upcase
    [@record.puzzle.name, kind].join " "
  end

  def singles_as_text
    average = @record.cubing_average
    best = average.best
    worst = average.worst

    average.singles.map do |s|
      time = if s.dnf?
        "DNF"
      else
        s.human_time(:unit => false) + (s.plus2? ? "+" : "")
      end

      if @record.amount > 1 and [best, worst].include? s
        "(#{time})"
      else
        time
      end
    end.join " "
  end
end
