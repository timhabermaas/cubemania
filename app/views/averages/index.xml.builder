xml.times :title => @user.name do
  xml << render(@averages)
end