xml.times :title => @user.name do
  xml << partial(@averages)
end