class AddScrambleToClocks < ActiveRecord::Migration
  def self.up
    add_column :clocks, :scramble, :string
  end

  def self.down
    remove_column :clocks, :scramble
  end
end
