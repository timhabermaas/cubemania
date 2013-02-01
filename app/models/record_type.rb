class RecordType < Struct.new(:full_name, :short_name, :count) # TODO add Mean/Average
  def self.all
    [
      RecordType.new("Single", "single", 1),
      RecordType.new("Average of 5", "avg5", 5),
      RecordType.new("Average of 12", "avg12", 12)
    ]
  end

  def self.by_count(count)
    all.find { |t| t.count == count }
  end

  def self.by_short_name(short_name)
    all.find { |t| t.short_name == short_name }
  end
end
