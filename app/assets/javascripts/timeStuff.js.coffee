root = exports ? this

root.checkKind = (index) ->
  $("#kinds ul").children("li").removeClass("checked").eq(index).addClass("checked")
  $("#puzzles > ul").animate {left: -100 * index + "%"}, "normal"

root.formatTime = (time) ->
  seconds = (time / 1000).toFixed(2);
  if seconds < 60
    "#{seconds}s"
  else
    minutes = Math.floor(seconds / 60)
    seconds = seconds - minutes * 60
    s = if seconds < 10 then "0" else ""
    "#{minutes}:#{s}#{seconds.toFixed(2)}min"
