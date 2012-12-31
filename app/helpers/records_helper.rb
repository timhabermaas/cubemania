module RecordsHelper
  class Type < Struct.new(:name, :short_name, :amount)
  end

  TYPES = [
    Type.new("Single", "single", 1),
    Type.new("Average of 5", "avg5", 5),
    Type.new("Average of 12", "avg12", 12)
  ]

  def record_type_tabs
    TYPES.map do |type|
      link_to type.name, puzzle_records_url(params[:puzzle_id], :type => type.short_name),
              :class => ("selected" if type.short_name == params[:type])
    end.join("\n").html_safe
  end
end