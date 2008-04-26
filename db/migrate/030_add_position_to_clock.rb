class AddPositionToClock < ActiveRecord::Migration
  def self.up
    add_column :clocks, :position, :integer
    averages = Average.find :all
    for average in averages
      index = 0;
      average.singles.each {|s| s.update_attribute :position, index; index += 1 }
    end
  end

  def self.down
    remove_column :clocks, :position
  end
end
