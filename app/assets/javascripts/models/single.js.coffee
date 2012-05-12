class Cubemania.Models.Single extends Backbone.Model

  defaults: ->
    "created_at": new Date().toISOString() # TODO replace with format("isoDateTime")?
    "updated_at": new Date().toISOString()

  initialize: (model, options) ->
    @setTimeFromHumanTime(model.human_time) if model.human_time?

  togglePlus2: ->
    if @plus2()
      @set("penalty", null)
      @set("time", @get("time") - 2000)
    else
      @set("penalty", "plus2")
      @set("time", @get("time") + 2000)

  toggleDnf: ->
    if @dnf()
      @set("penalty", null)
    else if @plus2()
      @set("penalty", "dnf")
      @set("time", @get("time") - 2000)
    else
      @set("penalty", "dnf")

  plus2: ->
    @get("penalty") == "plus2"

  dnf: ->
    @get("penalty") == "dnf"

  setTimeFromHumanTime: (human_time) ->
    [seconds, minutes, hours] = human_time.split(':').reverse()
    hours = if hours? then parseInt(hours) * 3600 else 0
    minutes = if minutes? then parseInt(minutes) * 60 else 0
    @set("time", (hours + minutes) * 1000 + parseFloat(seconds) * 1000)
