class Cubemania.Models.CurrentPuzzle
  constructor: ->
    @puzzle = null
    @name = null
    _.extend(this, Backbone.Events)

  getName: ->
    @puzzle.get("name") if @puzzle?

  set: (puzzle, trigger = true) ->
    # only trigger event if puzzle has changed or is null
    if trigger and (@puzzle == null or @puzzle.get("id") != puzzle.get("id"))
      @trigger("change", puzzle)

    @puzzle = puzzle
