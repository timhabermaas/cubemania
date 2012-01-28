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
end
