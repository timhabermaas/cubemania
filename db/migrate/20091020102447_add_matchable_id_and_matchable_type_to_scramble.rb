class AddMatchableIdAndMatchableTypeToScramble < ActiveRecord::Migration
  def self.up
    add_column :scrambles, :matchable_id, :integer, :null => false, :default => 0
    add_column :scrambles, :matchable_type, :string, :null => false, :default => 'Competition'
    Scramble.reset_column_information
    say_with_time "Updating matchable_ids" do
      Scramble.transaction do
        Scramble.find_each do |scramble|
          scramble.update_attribute :matchable_id, scramble.competition_id
          scramble.update_attribute :matchable_type, 'Competition'
        end
      end
    end
    remove_column :scrambles, :competition_id
  end

  def self.down
    remove_column :scrambles, :matchable_type
    remove_column :scrambles, :matchable_id
    add_column :scrambles, :competition_id, :integer
  end
end
