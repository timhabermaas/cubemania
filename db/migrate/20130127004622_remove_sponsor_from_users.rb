class RemoveSponsorFromUsers < ActiveRecord::Migration
  def up
    remove_column :users, :sponsor
  end

  def down
    add_column :users, :sponsor, :boolean, :null => false, :default => false
  end
end
