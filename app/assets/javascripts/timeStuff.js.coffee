root = exports ? this

root.formatTime = (time) ->
  seconds = (time / 1000).toFixed(2);
  if seconds < 60
    "#{seconds}s"
  else
    minutes = Math.floor(seconds / 60)
    seconds = seconds - minutes * 60
    s = if seconds < 10 then "0" else ""
    "#{minutes}:#{s}#{seconds.toFixed(2)}min"

root.average = (singles, size) ->
  singles = singles[0...size]
  dnfs = (single for single in singles when $(single).data("dnf"))

  return null if dnfs.length > 1 or singles.length < size

  solvedSingles = (single for single in singles when not $(single).data("dnf"))
  times = (parseInt($(single).data("time")) for single in solvedSingles)

  if dnfs.length == 1
    return (Array.sum(times) - Array.min(times)) / (times.length - 1)
  else # dnf.length == 0
    return (Array.sum(times) - Array.min(times)) / (times.length - 2)
