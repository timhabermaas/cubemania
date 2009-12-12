class AddWantsEmailsToUser < ActiveRecord::Migration
  def self.up
    add_column :users, :wants_emails, :boolean, :null => false, :default => false
  end

  def self.down
    remove_column :users, :wants_emails
  end
end
