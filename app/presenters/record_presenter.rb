class RecordPresenter
  def initialize(record)
    @record = record
  end

  def record_format
    amount = @record.amount
    case amount
    when 1
      "Single Record"
    else
      "Average of #{amount} Record"
    end
  end
end
