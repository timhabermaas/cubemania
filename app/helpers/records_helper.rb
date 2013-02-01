module RecordsHelper
  def record_type_tabs
    RecordType.all.map do |type|
      link_to type.full_name, puzzle_records_url(params[:puzzle_id], :type => type.short_name),
              :class => ("selected" if type.short_name == params[:type])
    end.join("\n").html_safe
  end
end
