class SetDefaultForSingleIds < ActiveRecord::Migration
  def self.up
    change_column :records, :single_ids, :string, :null => false, :default => ""
  end

  def self.down
    change_column :records, :single_ids, :string, :null => false, :default => nil
  end
end
