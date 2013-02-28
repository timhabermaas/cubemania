module ApplicationHelper
  def avatar_url(user, size)
    gravatar_id = Digest::MD5.hexdigest(user.email.downcase)
    "http://gravatar.com/avatar/#{gravatar_id}.png?s=#{size}"
  end

  def avatar_image(user, size)
    size = {
      :small => 25,
      :middle => 45,
      :large => 60
    }[size]
    image_tag avatar_url(user, size), :class => "profile-image"
  end

  def page_title
    if controller? :timers
      current_puzzle.full_name + " " + "Timer"
    elsif controller? :records and action? :index
      current_puzzle.full_name + " " + "Records"
    else
      params[:controller].titleize.singularize
    end
  end

  def action_label(new = 'Create', edit = 'Update')
    case params[:action].to_sym
      when :new, :create, :index
        new
      when :edit, :update, :show
        edit
    end
  end

  def admin_label(new = 'New', edit = 'Edit')
    case params[:action].to_sym
      when :index, :create
        new
      when :show, :update
        edit
      else
        edit # fix for competitions/1/add_average
    end.+ " #{params[:controller].singularize.titleize}"
  end

  def kinds
    @kinds ||= Kind.includes(:puzzles)
  end

  def controller?(*names)
    names.include? params[:controller].to_sym
  end

  def action?(*names)
    names.include? params[:action].to_sym
  end

  def display_subnavigation?
    controller? :records and action? :index or
      controller? :timers
  end

  def profile_page?
    current_page? user_path(current_user)
  end

  def edit?
    action? :edit, :update, :show
  end

  def current_puzzle
    @current_puzzle ||= Puzzle.find params[:puzzle_id], :include => :kind
  end

  def current_kind
    @current_kind ||= current_puzzle.kind
  end

  def current_puzzle?(puzzle)
    puzzle == current_puzzle
  end

  def current_kind?(kind)
    kind == current_kind
  end

  def current_kind_index
    kinds.index current_kind
  end

  def permit?
    case params[:controller].to_sym
      when :competitions
        edit? ? owner? : logged_in?
      when :users
        edit? and self?
      else
        false
    end
  end

  def ft(time, spacer = '', blank_time = '-:--.--')
    return blank_time if time.nil? # TODO make a TimePresenter.new(single)
    hs = (time / 10.0).round
    if hs >= 6000
      min = hs / 6000
      sec = (hs - min * 6000) / 100.0
      '%d:%05.2f' % [min, sec] + spacer + 'min' # 12.555 => "12.55"
    else
      '%.2f' % (hs.to_f / 100) + spacer + 's'
    end
  end

  def time_or_none(time)
    if time
      "<strong>#{ft time}</strong>"
    else
      "<small>None</small>"
    end.html_safe
  end

  def d(date)
    return "" if date.nil? # TODO move to presenter class
    date.strftime '%B %d, %Y'
  end

  def dt(datetime)
    return "" if datetime.nil? # TODO move to presenter class
    datetime.strftime '%B %d, %Y at %H:%M'
  end

  def time(time)
    return "" if time.nil?
    time.strftime '%H:%M'
  end

  def compare(my_time, other_time)
    if other_time.nil? and my_time.nil?
      "equal"
    elsif other_time.nil?
      "faster"
    elsif my_time.nil?
      "slower"
    elsif my_time < other_time
      "faster"
    elsif my_time > other_time
      "slower"
    else
      "equal"
    end
  end

  def m(text)
    RedCloth::new(text).to_html[3..-5].gsub("</p>\n<p>", "<br />").html_safe if text.present?
  end

  def format_scramble(text)
    text.present? ? text.gsub("\n", '<br />').html_safe : ""
  end
  alias_method :fs, :format_scramble

  def wca(id)
    'http://www.worldcubeassociation.org/results/p.php?i=' + id
  end

  def li_for(record, *args, &block)
    content_tag_for :li, record, *args, &block
  end

  def paginate(object, per_page = 100)
    object = object.paginate :page => params[:page], :per_page => per_page
  end

  def current_page_number
    params[:page].present? ? params[:page].to_i : 1
  end

  def next_page
    current_page_number + 1
  end

  def possessive(name)
    name + ('s' == name[-1,1] ? "'" : "'s")
  end

  def tim_path
    if u = User.find_by_slug("tim")
      user_path u
    else
      root_path
    end
  end
end
