class CreateKinds < ActiveRecord::Migration
  def self.up
    create_table :kinds do |t|
      t.string :name, :limit => 64, :null => false
    end
  end

  def self.down
    drop_table :kinds
  end
end
