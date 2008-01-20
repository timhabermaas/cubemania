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

ActiveRecord::Schema.define(:version => 2) do

  create_table "kinds", :force => true do |t|
    t.string   "name",       :default => "", :null => false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "puzzles", :force => true do |t|
    t.string   "name",       :limit => 42, :default => "", :null => false
    t.string   "image",      :limit => 42, :default => "", :null => false
    t.integer  "kind_id",                                  :null => false
    t.integer  "dimension",                                :null => false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  add_index "puzzles", ["kind_id", "dimension"], :name => "index_puzzles_on_kind_id_and_dimension"

end
