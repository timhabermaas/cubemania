class Cubemania.Models.CurrentPuzzle
  constructor: ->
    @puzzle = null
    @name = null
    _.extend(this, Backbone.Events)

  getName: ->
    @puzzle.get("name") if @puzzle?

  getFullName: ->
    @puzzle.get("name") + " " + @puzzle.get("kind").name

  set: (puzzle, trigger = true) ->
    # only trigger event if puzzle has changed or is null
    oldPuzzle = @puzzle
    @puzzle = puzzle
    if trigger and (oldPuzzle == null or oldPuzzle.get("id") != puzzle.get("id"))
      @trigger("change", puzzle)
