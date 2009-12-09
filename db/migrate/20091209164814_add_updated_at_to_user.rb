class AddUpdatedAtToUser < ActiveRecord::Migration
  def self.up
    add_column :users, :updated_at, :datetime
    User.all.each do |user|
      user.touch
    end
  end

  def self.down
    remove_column :users, :updated_at
  end
end
