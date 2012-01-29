root = exports ? this

jQuery ->
  if $("#timer").length
    timerStarted = false
    intervalId = null
    justStopped = false
    timerEnabled = true
    startTime = 0
    stopTime = 0

    $("#singles li a.dnf").bind "click", (event) ->
      time = $(this).parent().parent().find(".time")
      time.toggleClass("dnf")
      updateStatistics()

    $("#singles li a.plus2").bind "click", (event) ->
      time = $(this).parent().parent().find(".time")
      time.toggleClass("plus2")
      if time.hasClass("plus2")
        time.data("time", parseInt(time.data("time")) + 2000)
      else
        time.data("time", parseInt(time.data("time")) - 2000)
      time.text(formatTime parseInt(time.data("time")))
      updateStatistics()

    $("#timer a.toggle").bind "click", (event) ->
      $("#timer #new_single").toggle()
      $("#timer .time").toggle()
      $(this).toggleText("Changed your mind?", "Set times manually")
      if $(this).text() == "Changed your mind?"
        $("#single_human_time").focus()
      else
        $("#single_human_time").blur()
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
          startTimer() if timeSinceStopped() > 2000
        event.preventDefault()

    startTimer = ->
      timerStarted = true
      startTime = new Date().getTime()
      intervalId = setInterval(updateDisplay, 21)

    stopTimer = ->
      updateDisplay()
      stopTime = new Date().getTime()
      submitTime(currentTime())
      clearInterval(intervalId)
      timerStarted = false

    submitTime = (time) ->
      $("#timer #single_time").val(time)
      $("#timer #single_human_time").val("")
      $("#timer form").submit()

    currentTime = ->
      new Date().getTime() - startTime

    timeSinceStopped = ->
      new Date().getTime() - stopTime

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
      singles.removeClass("best").removeClass("worst")
      best(singles).addClass("best")
      worst(singles).addClass("worst")

    updateStatistics();
