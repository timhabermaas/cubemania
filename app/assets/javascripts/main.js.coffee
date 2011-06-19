tooltip = ->
  xOffset = 10
  yOffset = 20
  $(".tooltip").hover((e) ->
    return if this.title is ""
    this.t = this.title
    this.title = ""
    $("#tooltip").text this.t
    $("#tooltip").animate({top: (e.pageY - xOffset) + "px", left: (e.pageX + yOffset) + "px"}, {queue: false, duration: 400}).fadeIn("normal")
  , ->
    return if this.t is null
    this.title = this.t
    $("#tooltip").hide()
  )

$(document).ajaxComplete (e, request, options) ->
  message = request.getResponseHeader("X-Message")
  title = request.getResponseHeader("X-Title")
  if message?
    messages = message.split("@")
    titles = title.split("@")
    $.each messages, (index, item) ->
      $.gritter.add {
        title: if titles[index] is "1" then "Single Record" else "Average of #{titles[index]} Record"
        text: "You set a time of #{formatTime(parseInt(item))}!<br /><a href='#'>Share on Twitter</a>"
        image: $("#subnavigation #puzzles .checked .puzzle").css("background-image").replace(/^url|[\(\)]/g, '')
        time: 15000
      }

calculateDst = ->
  rightNow = new Date()
  jan1 = new Date(rightNow.getFullYear(), 0, 1, 0, 0, 0, 0)
  june1 = new Date(rightNow.getFullYear(), 6, 1, 0, 0, 0, 0)
  temp = jan1.toGMTString()
  jan2 = new Date(temp.substring(0, temp.lastIndexOf(" ") - 1))
  temp = june1.toGMTString()
  june2 = new Date(temp.substring(0, temp.lastIndexOf(" ") - 1))
  std_time_offset = (jan1 - jan2) / (1000 * 60 * 60)
  daylight_time_offset = (june1 - june2) / (1000 * 60 * 60)
  if std_time_offset is daylight_time_offset
    0
  else
    1

document.cookie = 'tz_offset=' + ((new Date()).getTimezoneOffset() + calculateDst() * 60)