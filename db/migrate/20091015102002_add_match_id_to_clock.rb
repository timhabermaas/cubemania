class AddMatchIdToClock < ActiveRecord::Migration
  def self.up
    add_column :clocks, :match_id, :integer
  end

  def self.down
    remove_column :clocks, :match_id
  end
end
