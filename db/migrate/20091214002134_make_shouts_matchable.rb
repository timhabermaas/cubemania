class MakeShoutsMatchable < ActiveRecord::Migration
  def self.up
    add_column :shouts, :matchable_id, :integer, :null => false, :default => 0
    add_column :shouts, :matchable_type, :string, :null => false, :default => 'Competition'
    say_with_time "Updating Scrambles#matchable_ids" do
      Shout.transaction do
        Shout.all.each do |s|
          s.update_attribute :matchable_id, s.competition_id
          s.update_attribute :matchable_type, 'Competition'
        end
      end
    end
    remove_column :shouts, :competition_id
  end

  def self.down
    add_column :shouts, :competition_id, :integer
    Shout.each do |s|
      if s.matchable_type == 'Competition'
        s.update_attribute :competition_id, s.matchable_id
      else
        s.destroy
      end
    end
    remove_column :shouts, :matchable_id
    remove_column :shouts, :matchable_type
  end
end
