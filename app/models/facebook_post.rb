require "singles_formatter"

class FacebookPost
  def initialize(record, formatter = SinglesFormatter.new(record.singles))
    @record = record
    @formatter = formatter
  end

  def body
    if @record.type.single?
      @record.scrambles.first
    else
      @formatter.as_text
    end
  end

  def caption
    "Keep track of your times and join Cubemania!"
  end

  def title
    "#{@record.user.name.capitalize} has a new #{@record.puzzle.full_name} " + @record.type.full_name + " record: " + @record.human_time
  end
end
