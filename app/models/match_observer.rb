class MatchObserver < ActiveRecord::Observer
  def after_save(match)
    if match.challenged? and match.opponent.wants_emails?
      UserMailer.deliver_match_mail(match.opponent, match)
    end
  end
end