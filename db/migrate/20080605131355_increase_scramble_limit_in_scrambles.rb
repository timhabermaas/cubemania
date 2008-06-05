class IncreaseScrambleLimitInScrambles < ActiveRecord::Migration
  def self.up
    change_column :scrambles, :scramble, :string, :limit => 1024, :null => false
  end

  def self.down
    change_column :scrambles, :scramble, :string, :null => false
  end
end
