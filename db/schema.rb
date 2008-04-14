# This file is auto-generated from the current state of the database. Instead of editing this file, 
# please use the migrations feature of ActiveRecord to incrementally modify your database, and
# then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your database schema. If you need
# to create the application database on another system, you should be using db:schema:load, not running
# all the migrations from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended to check this file into your version control system.

ActiveRecord::Schema.define(:version => 24) do

  create_table "clocks", :force => true do |t|
    t.integer  "time",                             :null => false
    t.integer  "puzzle_id",                        :null => false
    t.datetime "created_at"
    t.integer  "user_id",    :default => 0,        :null => false
    t.string   "scramble"
    t.string   "type",       :default => "Single", :null => false
    t.integer  "average_id"
    t.boolean  "record",     :default => false,    :null => false
    t.boolean  "dnf",        :default => false,    :null => false
  end

  add_index "clocks", ["user_id", "puzzle_id", "created_at"], :name => "index_clocks_on_user_id_and_puzzle_id_and_created_at"

  create_table "comments", :force => true do |t|
    t.text     "content",    :null => false
    t.integer  "post_id",    :null => false
    t.integer  "user_id",    :null => false
    t.datetime "created_at", :null => false
  end

  add_index "comments", ["post_id", "created_at"], :name => "index_comments_on_post_id_and_created_at"

  create_table "items", :force => true do |t|
    t.string "name",        :limit => 64,                 :null => false
    t.string "description",                               :null => false
    t.string "url",         :limit => 32, :default => "", :null => false
  end

  create_table "kinds", :force => true do |t|
    t.string "name",  :limit => 64, :null => false
    t.string "image", :limit => 64
  end

  create_table "posts", :force => true do |t|
    t.string   "title"
    t.text     "content"
    t.integer  "user_id"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "comments_count", :default => 0, :null => false
  end

  create_table "puzzles", :force => true do |t|
    t.string  "name",            :limit => 64,                 :null => false
    t.string  "image",           :limit => 64,                 :null => false
    t.integer "kind_id",                                       :null => false
    t.integer "scramble_length"
    t.integer "record_id"
    t.integer "attempt_count",                 :default => 1,  :null => false
    t.integer "countdown",                     :default => 15, :null => false
  end

  add_index "puzzles", ["kind_id", "name"], :name => "index_puzzles_on_kind_id_and_name", :unique => true

  create_table "users", :force => true do |t|
    t.string   "name",               :limit => 32,                     :null => false
    t.string   "email",              :limit => 32,                     :null => false
    t.string   "salt",               :limit => 8,                      :null => false
    t.string   "encrypted_password", :limit => 16,                     :null => false
    t.datetime "created_at"
    t.string   "role",               :limit => 16, :default => "user"
  end

  add_index "users", ["name", "encrypted_password"], :name => "index_users_on_name_and_encrypted_password", :unique => true

end
