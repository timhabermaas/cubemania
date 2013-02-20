class CreateActivities < ActiveRecord::Migration
  def change
    create_table :activities do |t|
      t.integer :trackable_id, :null => false
      t.integer :user_id, :null => false
      t.string :type, :null => false
      t.timestamps
    end
  end
end
