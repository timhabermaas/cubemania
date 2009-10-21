class UpdateIndexForScramble < ActiveRecord::Migration
  def self.up
    remove_index :scrambles, :name => "index_scrambles_on_competition_id_and_created_at_and_position"
    add_index :scrambles, [:matchable_id, :matchable_type, :created_at, :position], :name => "index_scrambles_on_matchable_and_created_at_and_position"
  end

  def self.down
    remove_index :scrambles, :name => "index_scrambles_on_matchable_and_created_at_and_position"
    add_index :scrambles, [:created_at, :position], :name => "index_scrambles_on_competition_id_and_created_at_and_position"
  end
end
