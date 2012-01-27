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

ActiveRecord::Schema.define(:version => 20120126233637) do

  create_table "authorizations", :force => true do |t|
    t.integer  "user_id",    :null => false
    t.string   "provider",   :null => false
    t.string   "uid",        :null => false
    t.string   "token",      :null => false
    t.string   "secret"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  create_table "averages", :force => true do |t|
    t.integer  "time",                              :null => false
    t.boolean  "dnf",            :default => false, :null => false
    t.integer  "user_id",                           :null => false
    t.integer  "puzzle_id",                         :null => false
    t.integer  "competition_id",                    :null => false
    t.string   "comment"
    t.datetime "created_at",                        :null => false
    t.datetime "updated_at",                        :null => false
  end

  create_table "comments", :force => true do |t|
    t.text     "content",    :null => false
    t.integer  "post_id",    :null => false
    t.integer  "user_id"
    t.datetime "created_at", :null => false
  end

  create_table "competitions", :force => true do |t|
    t.string   "name",           :limit => 64,                     :null => false
    t.string   "description"
    t.integer  "user_id",                                          :null => false
    t.datetime "created_at",                                       :null => false
    t.datetime "updated_at",                                       :null => false
    t.integer  "puzzle_id",                                        :null => false
    t.string   "repeat",         :limit => 32, :default => "once", :null => false
    t.boolean  "sticky",                       :default => false,  :null => false
    t.integer  "averages_count",               :default => 0,      :null => false
    t.string   "skill",          :limit => 32, :default => "all",  :null => false
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
  end

  add_index "delayed_jobs", ["priority", "run_at"], :name => "delayed_jobs_priority"

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

  create_table "puzzles", :force => true do |t|
    t.string  "name",            :limit => 64,                        :null => false
    t.integer "kind_id",                                              :null => false
    t.integer "scramble_length"
    t.integer "record_id"
    t.integer "attempt_count",                 :default => 1,         :null => false
    t.integer "countdown",                     :default => 15,        :null => false
    t.string  "average_format",                :default => "average", :null => false
    t.integer "version",                       :default => 0
    t.integer "css_position",                  :default => 0,         :null => false
    t.string  "slug"
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
  end

  create_table "records_singles", :id => false, :force => true do |t|
    t.integer "record_id", :null => false
    t.integer "single_id", :null => false
  end

  create_table "scrambles", :force => true do |t|
    t.string   "scramble",       :limit => 1024, :null => false
    t.integer  "position",                       :null => false
    t.integer  "competition_id",                 :null => false
    t.datetime "created_at"
  end

  create_table "shouts", :force => true do |t|
    t.string   "content",        :null => false
    t.integer  "competition_id", :null => false
    t.integer  "user_id"
    t.datetime "created_at"
  end

  create_table "singles", :force => true do |t|
    t.integer  "time",                                      :null => false
    t.integer  "puzzle_id",                                 :null => false
    t.datetime "created_at"
    t.integer  "user_id",                    :default => 0, :null => false
    t.string   "scramble",   :limit => 1024
    t.integer  "average_id"
    t.string   "comment"
    t.datetime "updated_at"
    t.string   "penalty",    :limit => 8
  end

  create_table "users", :force => true do |t|
    t.string   "name",               :limit => 32,                      :null => false
    t.string   "email",              :limit => 64,                      :null => false
    t.string   "salt",               :limit => 8,                       :null => false
    t.string   "encrypted_password",                                    :null => false
    t.datetime "created_at"
    t.string   "role",               :limit => 16,  :default => "user"
    t.string   "wca"
    t.boolean  "sponsor",                           :default => false,  :null => false
    t.string   "time_zone",          :limit => 100, :default => "UTC"
    t.boolean  "ignored",                           :default => false,  :null => false
    t.datetime "updated_at"
    t.boolean  "wants_emails",                      :default => false,  :null => false
    t.integer  "singles_count",                     :default => 0,      :null => false
    t.string   "slug"
  end

  add_index "users", ["slug"], :name => "index_users_on_slug"

end
