require 'digest/sha2'

class User < ActiveRecord::Base
  ENCRYPT = Digest::SHA256
  ROLES = %w{user moderator admin}

  attr_reader :password
  attr_accessor :bot_email
  attr_accessible :name, :email, :time_zone, :password, :password_confirmation, :wca, :bot_email, :wants_emails, :as => :user
  attr_accessible :name, :email, :time_zone, :password, :password_confirmation, :wca, :bot_email, :wants_emails, :ignored, :as => :moderator
  attr_accessible :name, :email, :time_zone, :password, :password_confirmation, :wca, :bot_email, :wants_emails, :ignored, :role, :sponsor, :as => :admin

  humanize :wasted_time => :time

  has_many :posts, :dependent => :nullify
  has_many :comments, :dependent => :nullify
  has_many :competitions, :dependent => :nullify
  has_many :shouts, :dependent => :nullify
  has_many :home_matches, :foreign_key => 'user_id', :class_name => 'Match', :dependent => :delete_all
  has_many :guest_matches, :foreign_key => 'opponent_id', :class_name => 'Match', :dependent => :delete_all
  def participances
    cols = %w(id name puzzle_id created_at repeat).map { |c| "competitions.#{c}"}.join(', ')
    Competition.joins(:averages).select("#{cols}, clocks.created_at as date").where(:user_id => self.id).group("#{cols}, clocks.created_at").all
  end
  has_many :singles, :dependent => :delete_all do
    def for(puzzle_id); where(:puzzle_id => puzzle_id).order('created_at desc'); end
    def best(puzzle_id); not_dnf.where(:puzzle_id => puzzle_id).order(:time).first; end
    def average(puzzle_id); not_dnf.where(:puzzle_id => puzzle_id).calculate(:average, :time); end
  end

  has_many :records, :include => { :puzzle => :kind }, :order => 'puzzles.name, kinds.name', :dependent => :delete_all do
    def for(puzzle_id, amount)
      find_by_puzzle_id_and_amount puzzle_id, amount
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
    user = find_by_name name
    user if user and user.encrypted_password == ENCRYPT.hexdigest(password + user.salt)
  end

  def self.max_singles_count
    maximum("singles_count")
  end

  def self.all_with_ranking
    User.all(:select => 'u1.id, u1.name, u1.points, u1.averages_count, COUNT(DISTINCT u2.points) AS rank',
             :from => 'users u1 JOIN users u2 ON (u1.points <= u2.points)',
             :group => 'u1.id',
             :order => 'rank')
  end

  def password=(password)
    @password = password
    if password_is_being_updated?
      self.salt = [Array.new(6) { rand(256).chr }.join].pack('m').chomp
      self.encrypted_password = ENCRYPT.hexdigest password + salt
    end
  end

  def reset_password!
    available_characters = ('a'..'z').to_a + ('A'..'Z').to_a + ('0'..'9').to_a
    self.password = Array.new(12) { available_characters.sample }.join
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

  def wasted_time
    singles.where('singles.dnf' => false).sum('singles.time')
  end

  def rank
    User.count(:all, :conditions => ['points > ?', self.points]) + 1
  end

  def streak
    _matches = matches.finished
    return 0 if _matches.empty? or _matches.first.winner.nil?
    result = (self == _matches.first.winner) ? 1 : -1
    for match in _matches.all.from 1
      break if match.winner.nil? or (result > 0 and match.winner != self) or (result < 0 and match.loser != self)
      result += 1 if result > 0 and match.winner == self
      result -= 1 if result < 0 and match.loser == self
    end
    result
  end

  def wins
    matches.finished.all.select do |match|
      match.winner == self
    end.size
  end

  def losses
    matches.finished.all.select do |match|
      match.loser == self
    end.size
  end

  def matches
    Match.for(self)
  end

  def as_json(options = {})
    options ||= {}
    options[:except] = (options[:except] || []) + [:encrypted_password, :salt, :ignored, :email, :created_at, :role, :sponsor, :wants_emails]
    super(options)
  end

  private
    def flush_passwords
      @password = @password_confirmation = nil
    end

    def password_is_being_updated?
      id.nil? or not password.blank?
    end

    def best_average(puzzle_id, amount = 5)
      # result = `bin/average #{id} #{puzzle_id} #{amount}`
      best = nil
      best_ra = nil
      ra = RollingAverage.new(amount)
      singles.for(puzzle_id).find_each do |single|
        ra << single
        if ra.average and (best.nil? or ra.average < best)
          best = ra.average
          best_ra = ra.dup
        end
      end
      best_ra # Average.new ?
    end
end