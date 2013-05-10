# TODO move to app/abilities
# TODO secure more; not logged in users shouldn't be able to list singles

class ApiAbility
  include CanCan::Ability

  def initialize(user)
    can :read, :all
    can :recent, Record

    if user.nil?
    elsif user.admin?
      can :manage, :all
    else
      can :create, Single
      can :chart, Single

      can [:update, :destroy], Single do |s|
        s.user_id == user.id
      end

      if user.moderator?
        can :block, User
      end
    end
  end
end
