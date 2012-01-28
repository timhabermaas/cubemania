module ApplicationHelper
  def page_title
    current_item.nil? ? params[:controller].titleize : current_item[:name]
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

  def navigation
    @navigation ||=
      [
        { :name => "Home", :path => root_path, :controller => "homes" },
        { :name => "Timer", :path => puzzle_timers_path(Puzzle.default), :controller => "timers" },
        { :name => "Competitions", :path => puzzle_competitions_path(Puzzle.default), :controller => "competitions" },
        { :name => "Users", :path => users_path, :controller => "users" },
        { :name => "Records", :path => puzzle_records_path(Puzzle.default), :controller => "records" } # TODO add default flag to puzzle
      ]
  end

  def current_item
    navigation.find { |item| current_item? item }
  end

  def kinds
    @kinds ||= Kind.includes(:puzzles)
  end

  def subnavigation_path(puzzle)
    url_for :puzzle_id => puzzle.slug, :type => params[:type], :controller => params[:controller]
  end

  def navigation_path(item)
    url_for :controller => item.controller, :action => item.action
  end

  def controller?(*names)
    names.include? params[:controller].to_sym
  end

  def action?(*names)
    names.include? params[:action].to_sym
  end

  def edit?
    action? :edit, :update, :show
  end

  def current_item?(item)
    controller? item[:controller].to_sym
  end

  def current_puzzle?(puzzle)
    if [puzzle.id.to_s, puzzle.slug].include? params[:puzzle_id]
      params[:kind_id] = puzzle.kind_id.to_s
      true
    else
      false
    end
  end

  def current_kind?(kind)
    params[:kind_id] == kind.id.to_s
  end

  def type?(type)
    params[:type] == type.to_s
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

  def d(date)
    date.strftime '%B %d, %Y'
  end

  def dt(datetime)
    datetime.strftime '%B %d, %Y at %H:%M'
  end

  def singles_as_string(average, spacer = ' ')
    average.singles.map { |s| s.dnf? ? 'DNF' : ft(s.time, spacer) }.join ', ' if average.respond_to? :singles
  end

  def flot_dt(time)
    time.to_i * 1000
  end

  def m(text)
    RedCloth::new(text).to_html.html_safe unless text.nil?
  end

  def s(number)
    number > 0 ? "+#{number}" : number.to_s
  end

  def result_tag(result, tag='cite')
    ("<#{tag} class=" +
    if result < 0
      '"negative">' + s(result)
    elsif result > 0
      '"positive">' + s(result)
    else
      '"neutral">0'
    end + "</#{tag}>").html_safe
  end

  def wca(id)
    'http://www.worldcubeassociation.org/results/p.php?i=' + id
  end

  def li_for(record, *args, &block)
    content_tag_for :li, record, *args, &block
  end

  def cache_key(attribute = nil)
    key = params.map{ |k, v| k.to_s + '/' + v.to_s}.sort
    key << attribute.to_s if attribute
    logger.info key.join('/')
    key.join('/')
  end

  def paginate(object, per_page = 100)
    object = object.paginate :page => params[:page], :per_page => per_page
  end

  def options_for_user_select
    users = User.joins(:singles).where("singles.puzzle_id" => params[:puzzle_id]).group("users.id").select("users.id, users.name")
    #users = User.find_by_sql ['SELECT u.id, u.name FROM singles r LEFT OUTER JOIN users u ON r.user_id = u.id WHERE r.puzzle_id = ? AND r.record = ? AND u.id <> ? ORDER BY u.name', params[:puzzle_id], true, current_user.id]
    options_for_select users.collect { |u| [u.name, "/users/#{u.id}/puzzles/#{params[:puzzle_id]}/singles.json"]}.unshift(['Compare with ...']) # was user_puzzle_averages_path(u.id, params[:puzzle_id], :format => :xml)
  end

  def possessive(name)
    name + ('s' == name[-1,1] ? "'" : "'s")
  end
end
