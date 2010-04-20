class ChangeStickyToNotNull < ActiveRecord::Migration
  def self.up
    Competition.all.each do |comp|
      comp.update_attribute :sticky, false if comp.sticky.nil?
    end
    change_column :competitions, :sticky, :boolean, :null => false, :default => false
  end

  def self.down
    change_column :competitions, :sticky, :boolean, :null => true
  end
end
