class AddAverageIdIndexToClocks < ActiveRecord::Migration
  def self.up
    add_index :clocks, [:average_id, :type, :position], :name => "index_clocks_on_average_id_and_type_and_position"
  end

  def self.down
    remove_index :clocks, :name => "index_clocks_on_average_id_and_type_and_position"
  end
end
