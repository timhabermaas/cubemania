xml.series :title => @user.name do
  xml << partial(@averages)
end