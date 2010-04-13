module ApplicationHelper
  def page_title
    current_item.nil? ? params[:controller].titleize : current_item.name
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
    end.+ " #{params[:controller].singularize.titleize}"
  end
  
  def navigation
    @navigation ||= Item.all
  end
  
  def current_item
    navigation.find { |item| current_item? item }
  end
  
  def kinds
    Kind.all
  end
  
  def subnavigation_path(puzzle)
    if controller? :matches
      url_for :puzzle_id => puzzle.id, :type => params[:type], :controller => params[:controller], :user_id => params[:user_id], :action => params[:action]
    else
      url_for :puzzle_id => puzzle.id, :type => params[:type], :controller => params[:controller]
    end
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
    controller? item.controller.to_sym
  end
  
  def current_puzzle?(puzzle)
    if params[:puzzle_id] == puzzle.id.to_s
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
  
  def repeat?(*repeats)
    repeats.include? self.repeat.to_sym
  end
  
  def repeat
    params[:repeat] || (@competition.repeat unless @competition.nil?)
  end
  
  def nominalize_repeat
    repeat == 'daily' ? 'day' : repeat[0..-3]
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
  
  def d(date)
    date.strftime '%B %d, %Y'
  end
  
  def t(time)
    hs = (time / 10.0).round
    if hs >= 6000
      min = hs / 6000
      sec = (hs - min * 6000) / 100.0
      '%d:%05.2f' % [min, sec] + ' min' # 12.555 => "12.55"
    else
      '%.2f' % (hs.to_f / 100) + ' s'
    end
  end

  def dt(datetime)
    datetime.strftime '%B %d, %Y at %H:%M'
  end
  
  def flot_dt(time)
    time.to_i * 1000
  end

  def m(text)
    RedCloth::new(text).to_html.html_safe
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
  
  def twitter(name)
    'http://twitter.com/' + name
  end
  
  def facebook(id)
    'http://www.facebook.com/profile.php?id=' + id.to_s
  end

  def singles_as_string(time)
    time.singles.map { |s| s.dnf? ? 'DNF' : t(s.time) }.join ', ' if time.respond_to? :singles
  end

  def li_for(record, *args, &block)
    content_tag_for :li, record, *args, &block
  end

  def options_for_user_select
    users = User.find_by_sql ['SELECT u.id, u.name FROM clocks r LEFT OUTER JOIN users u ON r.user_id = u.id WHERE r.puzzle_id = ? AND r.record = ? AND r.type = ? AND u.id <> ? ORDER BY u.name', params[:puzzle_id], true, 'Average', current_user.id]
    options_for_select users.collect { |u| [u.name, "/users/#{u.id}/puzzles/#{params[:puzzle_id]}/averages.xml"]}.unshift(['Compare with ...']) # was user_puzzle_averages_path(u.id, params[:puzzle_id], :format => :xml)
  end
end

module ActionView
  class Base
    @@field_error_proc = Proc.new do |html_tag, instance|
      error_class = 'error'
      if html_tag =~ /<(input|textarea|select)[^>]+class=/
        class_attribute = html_tag =~ /class=['"]/
        html_tag.insert(class_attribute + 7, "#{error_class} ")
      elsif html_tag =~ /<(input|textarea|select)/
        first_whitespace = html_tag =~ /\s/
        html_tag[first_whitespace] = " class='#{error_class}' "
      end
      html_tag
    end
  end
  module Helpers
    module FormHelper
      def error_message_on(object, method, prepend_text = '', append_text = '', css_class = 'error')
        if (obj = (object.respond_to?(:errors) ? object : instance_variable_get("@#{object}"))) && (errors = obj.errors.on(method))
          content_tag('span', "#{prepend_text}#{errors.is_a?(Array) ? errors.first : errors}#{append_text}", :class => 'error')
        else 
          ''
        end
      end
    end
  end
end