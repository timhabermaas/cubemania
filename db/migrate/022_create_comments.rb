class CreateComments < ActiveRecord::Migration
  def self.up
    create_table :comments do |t|
      t.text :content, :null => false
      t.belongs_to :commentable, :polymorphic => true, :null => false
      t.belongs_to :user, :null => false
      t.datetime :created_at, :null => false
    end
    add_index :comments, [:commentable_id, :commentable_type, :created_at]
  end

  def self.down
    remove_index :comments, [:commentable_id, :commentable_type, :created_at]
    drop_table :comments
  end
end