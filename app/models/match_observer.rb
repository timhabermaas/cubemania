class MatchObserver < ActiveRecord::Observer
  def after_save(match)
    if match.challenged?
      UserMailer.deliver_match_mail(match.opponent, match)
    end
  end
end