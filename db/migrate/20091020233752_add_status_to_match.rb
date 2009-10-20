class AddStatusToMatch < ActiveRecord::Migration
  def self.up
    add_column :matches, :status, :string, :null => false, :default => 'pending'
    for match in Match.all
      match.update_status!
    end
  end

  def self.down
    remove_column :matches, :status
  end
end
