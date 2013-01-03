class AverageType < Struct.new(:name, :short_name, :count)
  def self.all
    [
      AverageType.new("Single", "S", 1),
      AverageType.new("Average of 5", "AVG5", 5),
      AverageType.new("Average of 12", "AVG12", 12),
    ]
  end
end
