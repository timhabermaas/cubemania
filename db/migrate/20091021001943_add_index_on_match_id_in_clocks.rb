class AddIndexOnMatchIdInClocks < ActiveRecord::Migration
  def self.up
    ActiveRecord::Base.connection.select_all('set statement_timeout to
    600000')
    add_index :clocks, [:match_id, :user_id], :name => "index_clocks_on_match_id_and_user_id"
  end

  def self.down
    remove_index :clocks, :name => "index_clocks_on_match_id_and_user_id"
  end
end
