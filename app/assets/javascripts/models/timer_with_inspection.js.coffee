class Cubemania.TimerWithInspection
  constructor: (inspection = 15) ->
    _.extend(this, Backbone.Events)
    @state = "reset"
    @startedAt = new Date().getTime()
    @stoppedAt = @startedAt
    @inspection = inspection * 1000

  wantToStart: ->
    switch @state
      when "reset"
        if @stoppedForLongerThan(2)
          if @inspection?
            @countdownStartedAt = new Date().getTime()
            @setState "countdownStarted"
          else
            @startedAt = new Date().getTime()
            @setState "started"
      when "countdownStarted"
        @startedAt = new Date().getTime()
        @setState "started"
      when "stopped"
        @setState "reset"

  wantToStop: ->
    switch @state
      when "started"
        @stoppedAt = new Date().getTime()
        @setState "stopped"

  isRunning: ->
    @state == "started"

  isCountdownRunning: ->
    @state == "countdownStarted"

  penalty: ->
    @inspection - (@startedAt - @countdownStartedAt)

  stoppedForLongerThan: (time) ->
    (new Date().getTime() - @stoppedAt) > time * 1000

  setState: (state) ->
    @state = state
    @trigger(state, this)

  currentTime: ->
    if @isCountdownRunning()
      @inspection - (new Date().getTime() - @countdownStartedAt)
    else if @isRunning()
      new Date().getTime() - @startedAt
    else
      @stoppedAt - @startedAt
