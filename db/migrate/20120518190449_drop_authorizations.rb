class DropAuthorizations < ActiveRecord::Migration
  def up
    drop_table :authorizations
  end

  def down
    create_table :authorizations do |t|
      t.integer :user_id, :null => false
      t.string :provider, :null => false
      t.string :uid, :null => false
      t.string :token, :null => false
      t.string :secret

      t.timestamps
    end
  end
end
