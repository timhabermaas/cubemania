# This file is auto-generated from the current state of the database. Instead of editing this file, 
# please use the migrations feature of Active Record to incrementally modify your database, and
# then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your database schema. If you need
# to create the application database on another system, you should be using db:schema:load, not running
# all the migrations from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended to check this file into your version control system.

ActiveRecord::Schema.define(:version => 20091018143120) do

  create_table "clocks", :force => true do |t|
    t.integer  "time",                                                 :null => false
    t.integer  "puzzle_id",                                            :null => false
    t.datetime "created_at"
    t.integer  "user_id",                        :default => 0,        :null => false
    t.string   "scramble",       :limit => 1024
    t.string   "type",                           :default => "Single", :null => false
    t.integer  "average_id"
    t.boolean  "record",                         :default => false,    :null => false
    t.boolean  "dnf",                            :default => false,    :null => false
    t.string   "comment"
    t.integer  "position"
    t.integer  "competition_id"
    t.integer  "match_id"
  end

  add_index "clocks", ["average_id", "type", "position"], :name => "index_clocks_on_average_id_and_type_and_position"
  add_index "clocks", ["competition_id", "created_at", "type", "time"], :name => "index_clocks_on_competition_id_and_created_at_and_type_and_time"
  add_index "clocks", ["puzzle_id", "record", "type", "time"], :name => "index_clocks_on_puzzle_id_and_record_and_type_and_time"
  add_index "clocks", ["user_id", "puzzle_id", "type", "created_at"], :name => "index_clocks_on_user_id_and_puzzle_id_and_type_and_created_at"
  add_index "clocks", ["user_id", "record", "type"], :name => "index_clocks_on_user_id_and_record_and_type"

  create_table "comments", :force => true do |t|
    t.text     "content",    :null => false
    t.integer  "post_id",    :null => false
    t.integer  "user_id"
    t.datetime "created_at", :null => false
  end

  add_index "comments", ["post_id", "created_at"], :name => "index_comments_on_post_id_and_created_at"

  create_table "competitions", :force => true do |t|
    t.string   "name",           :limit => 64,                     :null => false
    t.string   "description"
    t.integer  "user_id"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "puzzle_id",                    :default => 0,      :null => false
    t.string   "repeat",         :limit => 32, :default => "once", :null => false
    t.boolean  "sticky"
    t.integer  "averages_count",               :default => 0,      :null => false
    t.string   "skill",          :limit => 32, :default => "all",  :null => false
  end

  add_index "competitions", ["puzzle_id", "sticky", "averages_count", "created_at"], :name => "index_competitions_on_p_id_and_sticky_and_a_count_and_c_at"

  create_table "items", :force => true do |t|
    t.string  "name",        :limit => 64,                      :null => false
    t.string  "description",                                    :null => false
    t.integer "position",                  :default => 0,       :null => false
    t.string  "controller",  :limit => 64, :default => "homes", :null => false
    t.string  "action",      :limit => 32, :default => "index", :null => false
  end

  add_index "items", ["position"], :name => "index_items_on_position"

  create_table "kinds", :force => true do |t|
    t.string   "name",               :limit => 64, :null => false
    t.string   "image_file_name"
    t.string   "image_content_type"
    t.integer  "image_file_size"
    t.datetime "image_updated_at"
  end

  create_table "matches", :force => true do |t|
    t.integer  "puzzle_id",                  :null => false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "user_id",     :default => 0, :null => false
    t.integer  "opponent_id", :default => 0, :null => false
  end

  create_table "posts", :force => true do |t|
    t.string   "title"
    t.text     "content"
    t.integer  "user_id"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "comments_count", :default => 0, :null => false
  end

  add_index "posts", ["created_at"], :name => "index_posts_on_created_at"

  create_table "puzzles", :force => true do |t|
    t.string   "name",               :limit => 64,                        :null => false
    t.integer  "kind_id",                                                 :null => false
    t.integer  "scramble_length"
    t.integer  "record_id"
    t.integer  "attempt_count",                    :default => 1,         :null => false
    t.integer  "countdown",                        :default => 15,        :null => false
    t.string   "average_format",                   :default => "average", :null => false
    t.string   "image_file_name"
    t.string   "image_content_type"
    t.integer  "image_file_size"
    t.datetime "image_updated_at"
  end

  add_index "puzzles", ["kind_id", "name"], :name => "index_puzzles_on_kind_id_and_name", :unique => true

  create_table "scrambles", :force => true do |t|
    t.string   "scramble",       :limit => 1024, :null => false
    t.integer  "position",                       :null => false
    t.integer  "competition_id",                 :null => false
    t.datetime "created_at"
  end

  add_index "scrambles", ["competition_id", "created_at", "position"], :name => "index_scrambles_on_competition_id_and_created_at_and_position"

  create_table "shouts", :force => true do |t|
    t.string   "content",        :null => false
    t.integer  "competition_id", :null => false
    t.integer  "user_id"
    t.datetime "created_at"
  end

  create_table "users", :force => true do |t|
    t.string   "name",               :limit => 32,                      :null => false
    t.string   "email",              :limit => 64,                      :null => false
    t.string   "salt",               :limit => 8,                       :null => false
    t.string   "encrypted_password",                                    :null => false
    t.datetime "created_at"
    t.string   "role",               :limit => 16,  :default => "user"
    t.integer  "averages_count",                    :default => 0,      :null => false
    t.string   "wca"
    t.boolean  "sponsor",                           :default => false,  :null => false
    t.string   "time_zone",          :limit => 100, :default => "UTC"
    t.boolean  "ignored",                           :default => false,  :null => false
  end

  add_index "users", ["averages_count"], :name => "index_users_on_averages_count"
  add_index "users", ["email"], :name => "index_users_on_email", :unique => true
  add_index "users", ["name", "encrypted_password"], :name => "index_users_on_name_and_encrypted_password", :unique => true

end
