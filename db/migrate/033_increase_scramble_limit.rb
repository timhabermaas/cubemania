class IncreaseScrambleLimit < ActiveRecord::Migration
  def self.up
    change_column :clocks, :scramble, :string, :limit => 1024
  end

  def self.down
    change_column :clocks, :scramble, :string
  end
end
