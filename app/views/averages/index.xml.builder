xml.chart :items => 20 do
  xml.series :title => @user.name do
    xml << partial(@averages)
  end
end