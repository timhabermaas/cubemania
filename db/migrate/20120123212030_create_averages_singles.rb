class CreateAveragesSingles < ActiveRecord::Migration
  def change
    create_table :averages_singles, :id => false do |t|
      t.integer :average_id, :null => false
      t.integer :single_id, :null => false
    end
  end
end
