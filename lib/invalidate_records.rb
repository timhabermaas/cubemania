class InvalidateRecords
  def self.for(single, record_class=Record)
    record_class.younger_than(single).each(&:destroy)
  end
end
