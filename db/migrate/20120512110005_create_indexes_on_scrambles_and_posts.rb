class CreateIndexesOnScramblesAndPosts < ActiveRecord::Migration
  def change
    add_index :posts, :created_at, :name => "index_posts_on_created_at"
    add_index :scrambles, [:competition_id, :created_at, :position], :name => "index_scrambles_on_competition_id_and_created_at_and_position"
  end
end
