class RecalculateRecordsHistory
  # singles must be in ascending order
  # (this means: first single is the oldest)
  def self.for(type, singles, old_record, record_class=Record)
    result = []

    while singles.size >= type.count
      recent_singles = singles[0...type.count]
      new_time = type.calculator.new(recent_singles).result

      if !new_time.nil? and (old_record.nil? or new_time < old_record.time)
       new_record = record_class.new(:time => new_time)
       result << new_record
       old_record = new_record
      end
      singles = singles[1..-1]
    end

    result
  end
end
