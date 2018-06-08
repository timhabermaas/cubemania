# encoding: UTF-8
# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended to check this file into your version control system.

ActiveRecord::Schema.define(:version => 20180608163513) do

  create_table "activities", :force => true do |t|
    t.integer  "trackable_id",                  :null => false
    t.integer  "user_id",                       :null => false
    t.string   "type",                          :null => false
    t.datetime "created_at",                    :null => false
    t.datetime "updated_at",                    :null => false
    t.integer  "comments_count", :default => 0, :null => false
  end

  create_table "comments", :force => true do |t|
    t.text     "content",          :null => false
    t.integer  "user_id"
    t.datetime "created_at",       :null => false
    t.integer  "commentable_id",   :null => false
    t.string   "commentable_type", :null => false
  end

  create_table "cubing_sessions", :force => true do |t|
    t.integer  "user_id"
    t.integer  "puzzle_id"
    t.text     "single_ids"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  create_table "delayed_jobs", :force => true do |t|
    t.integer  "priority",   :default => 0
    t.integer  "attempts",   :default => 0
    t.text     "handler"
    t.text     "last_error"
    t.datetime "run_at"
    t.datetime "locked_at"
    t.datetime "failed_at"
    t.string   "locked_by"
    t.datetime "created_at",                :null => false
    t.datetime "updated_at",                :null => false
    t.string   "queue"
  end

  add_index "delayed_jobs", ["priority", "run_at"], :name => "delayed_jobs_priority"

  create_table "followings", :force => true do |t|
    t.integer  "follower_id", :null => false
    t.integer  "followee_id", :null => false
    t.datetime "created_at",  :null => false
    t.datetime "updated_at",  :null => false
  end

  add_index "followings", ["followee_id"], :name => "index_followings_on_followee_id"
  add_index "followings", ["follower_id"], :name => "index_followings_on_follower_id"

  create_table "kinds", :force => true do |t|
    t.string  "name",         :limit => 64,                :null => false
    t.integer "css_position",               :default => 0, :null => false
    t.string  "short_name"
  end

  create_table "posts", :force => true do |t|
    t.string   "title"
    t.text     "content"
    t.integer  "user_id"
    t.datetime "created_at",                    :null => false
    t.datetime "updated_at",                    :null => false
    t.integer  "comments_count", :default => 0, :null => false
  end

  add_index "posts", ["created_at"], :name => "index_posts_on_created_at"

  create_table "puzzles", :force => true do |t|
    t.string   "name",            :limit => 64,                        :null => false
    t.integer  "kind_id",                                              :null => false
    t.integer  "scramble_length"
    t.integer  "attempt_count",                 :default => 1,         :null => false
    t.integer  "countdown",                     :default => 15,        :null => false
    t.string   "average_format",                :default => "average", :null => false
    t.integer  "version",                       :default => 0
    t.integer  "css_position",                  :default => 0,         :null => false
    t.string   "slug"
    t.datetime "created_at",                                           :null => false
    t.datetime "updated_at",                                           :null => false
  end

  add_index "puzzles", ["slug"], :name => "index_puzzles_on_slug", :unique => true

  create_table "records", :force => true do |t|
    t.integer  "time",       :null => false
    t.integer  "puzzle_id",  :null => false
    t.integer  "user_id",    :null => false
    t.integer  "amount",     :null => false
    t.datetime "set_at",     :null => false
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
    t.string   "comment"
  end

  add_index "records", ["puzzle_id", "amount", "time"], :name => "index_records_on_puzzle_id_and_amount_and_time"
  add_index "records", ["puzzle_id", "amount", "user_id"], :name => "index_unique_records_on_puzzle_id_amount_user_id", :unique => true

  create_table "records_singles", :id => false, :force => true do |t|
    t.integer "record_id", :null => false
    t.integer "single_id", :null => false
  end

  add_index "records_singles", ["record_id", "single_id"], :name => "index_records_singles_on_record_id_and_single_id"

  create_table "sessions", :force => true do |t|
    t.string   "session_id", :null => false
    t.text     "data"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "sessions", ["session_id"], :name => "index_sessions_on_session_id"
  add_index "sessions", ["updated_at"], :name => "index_sessions_on_updated_at"

  create_table "singles", :force => true do |t|
    t.integer  "time",                       :null => false
    t.integer  "puzzle_id",                  :null => false
    t.datetime "created_at",                 :null => false
    t.integer  "user_id",                    :null => false
    t.string   "scramble",   :limit => 1024
    t.string   "comment"
    t.datetime "updated_at",                 :null => false
    t.string   "penalty",    :limit => 8
  end

  add_index "singles", ["user_id", "puzzle_id", "created_at"], :name => "index_singles_on_user_id_and_puzzle_id_and_created_at"

  create_table "users", :force => true do |t|
    t.string   "name",               :limit => 32,                      :null => false
    t.string   "email",                                                 :null => false
    t.string   "salt",               :limit => 8,                       :null => false
    t.string   "encrypted_password",                                    :null => false
    t.datetime "created_at",                                            :null => false
    t.string   "role",               :limit => 16,  :default => "user"
    t.string   "wca"
    t.string   "time_zone",          :limit => 100, :default => "UTC",  :null => false
    t.boolean  "ignored",                           :default => false,  :null => false
    t.datetime "updated_at",                                            :null => false
    t.boolean  "wants_emails",                      :default => false,  :null => false
    t.integer  "singles_count",                     :default => 0,      :null => false
    t.string   "slug"
    t.integer  "wasted_time",        :limit => 8,   :default => 0,      :null => false
  end

  add_index "users", ["email"], :name => "index_users_on_email", :unique => true
  add_index "users", ["name", "encrypted_password"], :name => "index_users_on_name_and_encrypted_password", :unique => true
  add_index "users", ["singles_count"], :name => "index_users_on_singles_count"
  add_index "users", ["slug"], :name => "index_users_on_slug"

end
