class Cubemania.Models.Single extends Backbone.Model

  urlRoot: ->
    "/puzzles/" + @puzzle + "/singles"

  initialize: (puzzle) ->
    @puzzle = puzzle

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