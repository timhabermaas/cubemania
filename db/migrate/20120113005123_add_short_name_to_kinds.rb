class AddShortNameToKinds < ActiveRecord::Migration
  def change
    add_column :kinds, :short_name, :string
  end
end
