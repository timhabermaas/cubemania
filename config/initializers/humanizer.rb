module Humanizer
  def humanize (attributes_hash)
    attributes_hash.each do |attribute, type|
      if type == :time
        class_eval <<-ENDEVAL, __FILE__, __LINE__
          def human_#{attribute}(spacer = '')
            t = self.#{attribute} || 0
            hs = (t / 10.0).round
            if hs >= 360000
              hours = hs / 360000
              min = (hs - hours * 360000) / 6000
              sec = (hs - hours * 360000 - min * 6000) / 100.0
              '%d:%02d:%05.2f' % [hours, min, sec] + spacer + 'h'
            elsif hs >= 6000
              min = hs / 6000
              sec = (hs - min * 6000) / 100.0
              '%d:%05.2f' % [min, sec] + spacer + 'min' # 12.555 => "12.55"
            else
              '%.2f' % (hs.to_f / 100) + spacer + 's'
            end
          end
        ENDEVAL
      else
        raise RuntimeError.new("Unknown type for humanize. Use :time.")
      end
    end
  end
end

class ActiveRecord::Base
  extend Humanizer
end