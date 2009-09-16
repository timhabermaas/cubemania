xml.times :title => @user.name do
  xml << partial(@averages).to_s
end