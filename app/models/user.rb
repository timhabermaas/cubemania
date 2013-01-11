require 'digest/sha2'

class User < ActiveRecord::Base
  extend FriendlyId

  ENCRYPT = Digest::SHA256
  ROLES = %w{user moderator admin}

  friendly_id :name, :use => :slugged

  attr_reader :password
  attr_accessor :bot_email
  attr_accessible :name, :email, :time_zone, :password, :password_confirmation, :wca, :bot_email
  attr_accessible :name, :email, :time_zone, :password, :password_confirmation, :wca, :bot_email, :wants_emails, :as => :user
  attr_accessible :name, :email, :time_zone, :password, :password_confirmation, :wca, :bot_email, :wants_emails, :ignored, :as => :moderator
  attr_accessible :name, :email, :time_zone, :password, :password_confirmation, :wca, :bot_email, :wants_emails, :ignored, :role, :sponsor, :as => :admin

  has_many :posts, :dependent => :nullify
  has_many :comments, :dependent => :nullify
  has_many :singles, :dependent => :delete_all do
    def for(puzzle); where(:puzzle_id => puzzle.id).order('created_at desc'); end
    def best(puzzle); not_dnf.where(:puzzle_id => puzzle.id).order(:time).first; end
    def average(puzzle); not_dnf.where(:puzzle_id => puzzle.id).calculate(:average, :time); end
  end

  has_many :records, :include => { :puzzle => :kind }, :order => 'puzzles.name, kinds.name', :dependent => :delete_all do
    def for(puzzle, amount)
      find_by_puzzle_id_and_amount puzzle.id, amount
    end
  end

  scope :active, where('singles_count > 0')

  validates_presence_of :name, :email, :encrypted_password, :salt, :role
  validates_presence_of :password, :password_confirmation, :if => :password_is_being_updated?
  validates_uniqueness_of :name, :email, :case_sensitive => false, :message => 'is already in use by another user'
  validates_format_of :name, :with => /^([a-z0-9_]{2,16})$/i, :message => 'must be 2 to 16 letters, numbers or underscores and have no spaces'
  validates_exclusion_of :name, :in => %w(admin moderator), :message => "you don't belong here"
  validates_format_of :email, :with => /^\A([^@\s]+)@((?:[-a-z0-9]+\.)+[a-z]{2,})\Z$/i, :message => 'must be valid'
  validates_length_of :bot_email, :is => 0, :message => 'bots must not register', :if => Proc.new { |user| user.new_record? }
  validates_format_of :wca, :with => /[0-9]{4}[A-Z]{4}[0-9]{2}/, :message => 'is not a valid WCA ID', :unless => Proc.new { |user| user.wca.blank? }
  validates_format_of :password, :with => /^([\x20-\x7E]){4,16}$/, :message => 'must be 4 to 16 characters', :if => :password_is_being_updated?
  validates_confirmation_of :password, :message => 'should match confirmation'
  validates_inclusion_of :role, :in => ROLES

  after_save :flush_passwords

  def self.authorize(name, password)
    user = where("lower(name) = ?", name.downcase).first
    user if user and user.encrypted_password == ENCRYPT.hexdigest(password + user.salt)
  end

  def self.max_singles_count
    maximum("singles_count")
  end

  def best_average(puzzle, amount)
    result = CubingAverage.new [] # is basically a dnf average due to implementation of CubingAverage
    current_average = RollingAverage.new amount

    singles.for(puzzle).each do |single| # TODO each may be too hardcore
      current_average << single
      if current_average < result
        result = CubingAverage.new(current_average.singles.clone) # TODO use CubingAverage's initialize_copy
      end
    end

    result
  end

  def password=(password)
    @password = password
    if password_is_being_updated?
      self.salt = [Array.new(6) { rand(256).chr }.join].pack('m').chomp
      self.encrypted_password = ENCRYPT.hexdigest password + salt
    end
  end

  def reset_password!
    new_password = SecureRandom.hex(6)
    self.password = new_password
    self.password_confirmation = new_password
    save!
    new_password
  end

  def block!
    update_attribute :ignored, true
  end

  def unblock!
    update_attribute :ignored, false
  end

  def role?(required_role, request_id, object)
    if admin?
      true
    else
      case required_role
        when :self
          id.to_s == request_id
        when :owner
          id == object.user_id
        else
          role.to_sym == required_role
      end
    end
  end

  def admin?
    role.to_sym == :admin
  end

  def moderator?
    role.to_sym == :moderator
  end

  def activity(max)
    if max.zero?
      1
    else
      singles.size / max.to_f
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
