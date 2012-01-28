class AddCommentToRecords < ActiveRecord::Migration
  def change
    add_column :records, :comment, :string
  end
end
