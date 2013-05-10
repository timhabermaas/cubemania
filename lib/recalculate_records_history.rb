class RecalculateRecordsHistory
  # singles must be in ascending order
  # (this means: first single is the oldest)
  def self.for(type, singles, old_record=nil, record_class=Record)
    result = []

    while singles.size >= type.count
      recent_singles = singles[0...type.count]
      new_time = type.calculator.new(recent_singles).result

      if !new_time.nil? and (old_record.nil? or new_time < old_record.time)
       new_record = record_class.build_from_singles_and_type_and_time(recent_singles, type, new_time)
       result << new_record
       old_record = new_record
      end
      singles = singles[1..-1]
    end

    result
  end

  def self.for!(type, singles, old_record=nil, record_class=Record)
    records = self.for(type, singles, old_record, record_class=Record)
    records.each(&:save)
  end
end
