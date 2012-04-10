root = exports ? this

root.formatTime = (time) ->
  return "-:--.--" unless time?
  seconds = Math.round(time / 10) / 100;
  if seconds < 60
    "#{seconds.toFixed(2)}s"
  else
    minutes = Math.floor(seconds / 60)
    seconds = seconds - minutes * 60
    s = if seconds < 10 then "0" else ""
    "#{minutes}:#{s}#{seconds.toFixed(2)}min"

root.formatDate = (date) ->
  d = new Date(date)
  "#{d.getMonthName()} #{d.getDate()}, #{d.getFullYear()}" # March 8, 2010

root.average = (singles, size) ->
  singles = singles[0...size]
  dnfs = (single for single in singles when $(single).hasClass("dnf"))

  return null if dnfs.length > 1 or singles.length < size

  solvedSingles = (single for single in singles when not $(single).hasClass("dnf"))
  times = rawTimes solvedSingles

  if dnfs.length == 1
    return (Array.sum(times) - Array.min(times)) / (times.length - 1)
  else # dnf.length == 0
    return (Array.sum(times) - Array.min(times) - Array.max(times)) / (times.length - 2)

root.rawTimes = (singles) ->
  (parseInt($(single).data("time")) for single in singles)

root.worst = (singles) ->
  dnfSingles = (single for single in singles when $(single).hasClass("dnf"))
  if dnfSingles.length > 0
    return $(dnfSingles[dnfSingles.length - 1])
  else
    times = rawTimes singles
    max = Array.max times
    index = times.indexOf max
    return $(singles[index]);

root.best = (singles) ->
  solvedSingles = (single for single in singles when not $(single).hasClass("dnf"))
  if solvedSingles.length == 0
    return $(null);
  else
    times = rawTimes solvedSingles
    min = Array.min times
    index = times.indexOf min
    return $(solvedSingles[index])
