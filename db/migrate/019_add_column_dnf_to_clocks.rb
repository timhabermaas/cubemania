class AddColumnDnfToClocks < ActiveRecord::Migration
  def self.up
    add_column :clocks, :dnf, :boolean, :null => false, :default => false
  end

  def self.down
    remove_column :clocks, :dnf
  end
end
