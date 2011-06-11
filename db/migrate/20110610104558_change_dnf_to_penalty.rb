class ChangeDnfToPenalty < ActiveRecord::Migration
  def up
    add_column :singles, :penalty, :string, :limit => 8
    Single.reset_column_information
    Single.where(:dnf => true).update_all(:penalty => "dnf")
    remove_column :singles, :dnf
  end

  def down
    add_column :singles, :dnf, :boolean, :null => false, :default => false
    Single.reset_column_information
    Single.find_each do |single|
      single.update_attribute(:dnf, true) if single.penalty == "dnf"
    end
    remove_column :singles, :penalty
  end
end
