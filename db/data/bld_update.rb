Average.transaction do
  kind = Kind.find_by_name 'blindfolded'
  users = User.find :all
  kind.puzzles.each do |p|
    averages = Average.find_all_by_puzzle_id p.id
    averages.each do |a|
      a.update_attribute :time, a.singles.reject(&:dnf).empty? ? a.singles.sort_by(&:time).first.time : a.singles.sort_by(&:time).reject(&:dnf).first.time
      a.update_attribute :dnf, a.singles.reject(&:dnf).empty?
      a.update_attribute :record, false
    end
    users.each do |u|
      u.averages.for(p.id).sort_by(&:time).reject(&:dnf).first.update_attribute :record, true unless u.averages.for(p.id).reject(&:dnf).empty?
    end
  end
end