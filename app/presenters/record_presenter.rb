class RecordPresenter
  def initialize(record)
    @record = record
  end

  def record_type
    amount = @record.amount
    case amount
    when 1
      "Single"
    else
      "Average of #{amount}"
    end
  end

  def flash_message
    "You have a new #{record_type.downcase} record: <strong>#{@record.human_time}</strong>!"
  end
end
