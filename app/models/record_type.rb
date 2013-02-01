class RecordType < Struct.new(:name, :short_name, :count)
  def self.all
    [
      RecordType.new("Single", "S", 1),
      RecordType.new("Average of 5", "AVG5", 5),
      RecordType.new("Average of 12", "AVG12", 12),
    ]
  end
end
