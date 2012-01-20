Day = Struct.new(:year, :month, :day)
Month = Struct.new(:year, :month)

class Catalog
  def self.by_day(singles)
    singles.group_by do |single|
      date = single.created_at
      Day.new(date.year, date.month, date.day)
    end
  end

  def self.by_month(singles)
    singles.group_by do |single|
      date = single.created_at
      Month.new(date.year, date.month)
    end
  end
end
