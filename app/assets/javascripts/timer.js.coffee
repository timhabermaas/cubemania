root = exports ? this

jQuery ->
  if $("#timer").length
    timerStarted = false
    intervalId = null
    justStopped = false
    timerEnabled = true
    startTime = 0

    $("#timer .toggle").bind "click", (event) ->
      $("#timer #new_single").toggle()
      $("#timer .time").toggle()
      if $(this).text() == "Set times manually"
        $(this).text("Changed your mind?")
        $("#single_human_time").focus()
      else
        $(this).text("Set times manually")
      event.preventDefault()

    $("textarea, input[type=text]").bind('focus', ->
      timerEnabled = false
    )

    $("textarea, input[type=text]").bind('blur', ->
      timerEnabled = true
    )

    $(document).keydown (event) ->
      if event.keyCode == 32 and timerEnabled
        if timerStarted
          stopTimer()
          justStopped = true
        else
          $("#timer .time").addClass("starting")
        event.preventDefault()

    $(document).keyup (event) ->
      if event.keyCode == 32 and !timerStarted and timerEnabled
        if justStopped
          justStopped = false
        else
          $("#timer .time").removeClass("starting")
          startTimer()
        event.preventDefault()

    startTimer = ->
      timerStarted = true
      startTime = new Date().getTime()
      intervalId = setInterval(updateDisplay, 21)

    stopTimer = ->
      updateDisplay()
      submitTime(currentTime())
      clearInterval(intervalId)
      timerStarted = false

    submitTime = (time) ->
      $("#timer #single_time").val(time)
      $("#timer #single_human_time").val("")
      $("#timer form").submit()

    currentTime = ->
      new Date().getTime() - startTime

    updateDisplay = ->
      $("#timer .time").text(formatTime(currentTime()))

    getSingles = ->
      $("#singles ol .time")

    root.updateStatistics = ->
      singles = getSingles()
      cur5 = average(singles, 5)
      cur12 = average(singles, 12)

      $("#stats .current strong.avg5").text(formatTime(cur5));
      $("#stats .current strong.avg12").text(formatTime(cur12))
