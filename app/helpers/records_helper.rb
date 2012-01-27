module RecordsHelper
  def record_format(record)
    amount = record.amount
    case amount
    when 1
      "Single Record"
    else
      "Average of #{amount} Record"
    end
  end
end
