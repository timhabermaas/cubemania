class Cubemania.Timer
  constructor: ->
    _.extend(this, Backbone.Events)
    @reset()

  isRunning: ->
    @running

  start: (trigger = true) ->
    @startTime = new Date().getTime()
    @running = true
    @trigger("started", this)

  stop: (trigger = false) ->
    @stopTime = new Date().getTime()
    @running = false
    @trigger("stopped", this)

  reset: ->
    @startTime = 0
    @stopTime = 0
    @running = false
    @trigger("reset", this)

  currentTime: ->
    if @isRunning()
      new Date().getTime() - @startTime
    else
      @stopTime - @startTime

  timeSinceStopped: ->
    new Date().getTime() - @stopTime

###

root = exports ? this

jQuery ->
  if $("#timer").length
    timerStarted = false
    intervalId = null
    justStopped = false
    timerEnabled = true
    startTime = 0
    stopTime = 0

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
