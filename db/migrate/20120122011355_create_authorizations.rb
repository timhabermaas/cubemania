class CreateAuthorizations < ActiveRecord::Migration
  def change
    create_table :authorizations do |t|
      t.integer :user_id, :null => false
      t.string :provider, :null => false
      t.string :uid, :null => false

      t.timestamps
    end
  end
end
