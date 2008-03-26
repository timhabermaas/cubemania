require 'digest/sha2'

class User < ActiveRecord::Base
  ENCRYPT = Digest::SHA256

  attr_reader :password

  has_many :clocks, :order => 'created_at desc'

  validates_uniqueness_of :name, :email, :case_sensitive => false, :message => 'is already in use by another user'
  validates_format_of :name, :with => /^([a-z0-9_]{2,16})$/i, :message => 'must be 2 to 16 letters, numbers or underscores and have no spaces'
  validates_format_of :email, :with => /^\A([^@\s]+)@((?:[-a-z0-9]+\.)+[a-z]{2,})\Z$/i, :message => 'must be valid'
  validates_format_of :password, :with => /^([\x20-\x7E]){4,16}$/, :message => 'must be 4 to 16 characters', :if => :password_is_being_updated?
  validates_confirmation_of :password

  after_save :flush_passwords

  def self.find_by_name_and_password(name, password)
    user = find_by_name name
    user if user and user.encrypted_password == ENCRYPT.hexdigest(password + user.salt)
  end

  def password=(password)
    @password = password
    if password_is_being_updated?
      self.salt = [Array.new(6) { rand(256).chr }.join].pack('m').chomp
      self.encrypted_password = ENCRYPT.hexdigest password + salt
    end
  end

  def role?(role, request_id)
    if role.to_sym == :self
      id.to_s == request_id
    else
      true
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