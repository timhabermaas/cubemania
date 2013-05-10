require "mean"
require "average"

class RecordType < Struct.new(:full_name, :short_name, :calculator, :count) # TODO add Mean/Average
  def self.all
    [
      RecordType.new("Single", "single", Mean, 1),
      RecordType.new("Average of 5", "avg5", Average, 5),
      RecordType.new("Average of 12", "avg12", Average, 12),
      RecordType.new("Mean of 100", "mean100", Mean, 100)
    ]
  end

  def self.counts
    all.map(&:count)
  end

  def self.max_count
    counts.max
  end

  def self.by_count(count)
    all.find { |t| t.count == count }
  end

  def self.by_short_name(short_name)
    all.find { |t| t.short_name == short_name }
  end

  def single?
    count == 1
  end
end
