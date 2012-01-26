class ChangeDnfToPenalty < ActiveRecord::Migration
  def up
    add_column :singles, :penalty, :string, :limit => 8
    Single.reset_column_information
    say_with_time "set penalty to 'dnf' for all dnfs" do
      Single.find_by_sql("UPDATE singles s SET penalty='dnf' WHERE s.dnf=1")
    end
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
