class UpdateIndexes < ActiveRecord::Migration
  def self.up
    add_index :competitions, [:puzzle_id, :sticky, :averages_count, :created_at], :name => 'index_competitions_on_p_id_and_sticky_and_a_count_and_c_at'
    add_index :scrambles, [:competition_id, :created_at, :position]
    add_index :clocks, [:competition_id, :created_at, :type, :time]
  end

  def self.down
    remove_index :competitions, :name => 'index_competitions_on_p_id_and_sticky_and_a_count_and_c_at'
    remove_index :scrambles, :column => [:competition_id, :created_at, :position]
    remove_index :clocks, :column => [:competition_id, :created_at, :type, :time]
  end
end