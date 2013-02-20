class Feed
  def initialize(user)
    @user = user
  end

  def activities
    Activity.where(:user_id => @user.followee_ids + [@user.id]).
             order("created_at desc")
  end
end
