module ApplicationHelper
  def page_title
    params[:controller].titleize
  end
  
  def action_label(new = 'Create', edit = 'Update')
    case params[:action].to_sym
      when :new, :create
        new
      when :edit, :update
        edit
    end
  end
  
  def controller?(*names)
    names.include? params[:controller].to_sym
  end
  
  def action?(*names)
    names.include? params[:action].to_sym
  end
  
  def current_item?(item)
    controller? item.url.split('/').last.to_sym
  end
  
  def current_puzzle?(puzzle)
    params[:puzzle_id] == puzzle.id.to_s
  end
  
  def current_kind?(kind)
    params[:kind_id] == kind.id.to_s
  end
  
  def type?(type)
    params[:type] == type.to_s
  end
  
  def d(date)
    date.strftime '%B %d, %Y'
  end
  
  def t(time)
    if time > 60000
      min = time / 60000
      sec = (time - min * 60000) / 1000.0
      '%d:%05.2f' % [min, sec] + ' min'
    else
      '%.2f' % (time.to_f / 1000) + ' s'
    end
  end
  
  def dt(datetime)
    datetime.strftime '%B %d, %Y at %H:%M'
  end
  
  def m(text)
    markdown text
  end
  
  def li_for(record, *args, &block)
    content_tag_for :li, record, *args, &block
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