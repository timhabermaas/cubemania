class Cubemania.TimerWithInspection
  constructor: (inspection) ->
    _.extend(this, Backbone.Events)
    @reset(inspection)

  reset: (inspection) ->
    @state = "reset"
    @startedAt = new Date().getTime() - 10000
    @stoppedAt = @startedAt
    @inspection = inspection * 1000

  wantToStart: ->
    switch @state
      when "isAboutToStartCountdown"
        @countdownStartedAt = new Date().getTime()
        @setState "countdownStarted"
      when "isAboutToStart"
        @startedAt = new Date().getTime()
        @setState "started"
      when "stopped"
        @setState "reset"

  wantToStop: ->
    switch @state
      when "reset"
        if @stoppedForLongerThan(2)
          if @hasInspection()
            @setState "isAboutToStartCountdown"
          else
            @setState "isAboutToStart"
      when "countdownStarted"
        @setState "isAboutToStart"
      when "started"
        @stoppedAt = new Date().getTime()
        @setState "stopped"

  isReset: ->
    @state == "reset"

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
    @trigger state, this

  hasInspection: ->
    @inspection > 0

  currentTime: ->
    if @isCountdownRunning()
      @inspection - (new Date().getTime() - @countdownStartedAt)
    else if @isRunning()
      new Date().getTime() - @startedAt
    else
      @stoppedAt - @startedAt
