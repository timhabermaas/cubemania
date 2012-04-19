class Cubemania.Models.Single extends Backbone.Model

  defaults: ->
    "created_at": new Date().toISOString()
    "updated_at": new Date().toISOString()

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