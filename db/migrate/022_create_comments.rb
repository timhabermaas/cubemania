class CreateComments < ActiveRecord::Migration
  def self.up
    create_table :comments do |t|
      t.text :content, :null => false
      t.belongs_to :post, :null => false
      t.belongs_to :user, :null => false
      t.datetime :created_at, :null => false
    end
  end

  def self.down
    drop_table :comments
  end
end