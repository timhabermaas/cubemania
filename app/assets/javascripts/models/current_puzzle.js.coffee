class Cubemania.Models.CurrentPuzzle
  constructor: ->
    @puzzle = null
    @name = null
    _.extend(this, Backbone.Events)

  set: (puzzle) ->
    # only trigger event if puzzle has changed or is null
    if @puzzle == null or @puzzle.get("id") != puzzle.get("id")
      @puzzle = puzzle
      @name = @puzzle.get("name")
      @trigger("change", @puzzle)
