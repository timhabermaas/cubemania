require 'digest/sha2'

class User < ActiveRecord::Base
  ENCRYPT = Digest::SHA256

  attr_reader :password
  attr_accessor :bot_email

  has_many :posts, :dependent => :nullify
  has_many :comments, :dependent => :nullify
  has_many :clocks
  has_many :singles, :order => 'created_at desc', :dependent => :delete_all do
    def for(puzzle_id); find_all_by_puzzle_id puzzle_id, :order => 'time'; end
    def record(puzzle_id); find_by_puzzle_id_and_record puzzle_id, true; end
    def records; find_all_by_record true; end
  end
  has_many :averages, :order => 'created_at desc', :dependent => :delete_all do
    def for(puzzle_id); find_all_by_puzzle_id puzzle_id; end
    def record(puzzle_id); find_by_puzzle_id_and_record puzzle_id, true; end
    def records; find_all_by_record true, :include => { :puzzle => :kind }, :order => 'puzzles.name'; end
  end

  validates_uniqueness_of :name, :email, :case_sensitive => false, :message => 'is already in use by another user'
  validates_format_of :name, :with => /^([a-z0-9_]{2,16})$/i, :message => 'must be 2 to 16 letters, numbers or underscores and have no spaces'
  validates_exclusion_of :name, :in => %w(admin moderator), :message => "you don't belong here"
  validates_format_of :email, :with => /^\A([^@\s]+)@((?:[-a-z0-9]+\.)+[a-z]{2,})\Z$/i, :message => 'must be valid'
  validates_length_of :bot_email, :is => 0, :message => 'bots must not register'
  validates_format_of :password, :with => /^([\x20-\x7E]){4,16}$/, :message => 'must be 4 to 16 characters', :if => :password_is_being_updated?
  validates_confirmation_of :password

  after_save :flush_passwords

  def self.find_by_name_and_password(name, password)
    user = find_by_name name
    user if user and user.encrypted_password == ENCRYPT.hexdigest(password + user.salt)
  end

  def self.max_averages_count
    find(:first, :select => 'averages_count', :order => 'averages_count desc').averages_count
  end

  def password=(password)
    @password = password
    if password_is_being_updated?
      self.salt = [Array.new(6) { rand(256).chr }.join].pack('m').chomp
      self.encrypted_password = ENCRYPT.hexdigest password + salt
    end
  end

  def role?(required_role, request_id)
    if admin?
      true
    else
      if required_role == :self
        id.to_s == request_id
      else
        role.to_sym == required_role
      end
    end
  end

  def admin?
    role.to_sym == :admin
  end

  def activity(max)
    if max.zero?
      1
    else
      averages.size / max.to_f
    end
  end

  private
    def flush_passwords
      @password = @password_confirmation = nil
    end

    def password_is_being_updated?
      id.nil? or not password.blank?
    end
end