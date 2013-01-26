class Cubemania.Timer
  constructor: ->
    _.extend(this, Backbone.Events)
    @reset()

  isRunning: ->
    @running

  start: (trigger = true) ->
    @startTime = new Date().getTime()
    @running = true
    @trigger("started", this) if trigger

  stop: (trigger = true) ->
    @stopTime = new Date().getTime()
    @running = false
    @trigger("stopped", this) if trigger

  reset: (trigger = true) ->
    @startTime = 0
    @stopTime = 0
    @running = false
    @trigger("reset", this) if trigger

  currentTime: ->
    if @isRunning()
      new Date().getTime() - @startTime
    else
      @stopTime - @startTime

  timeSinceStopped: ->
    new Date().getTime() - @stopTime
