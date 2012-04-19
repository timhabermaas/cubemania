class Cubemania.Collections.Kinds extends Backbone.Collection
  model: Cubemania.Models.Kind

  puzzles: ->
    puzzleModels = (k.puzzles.models for k in @models)
    new Cubemania.Collections.Puzzles(_.flatten(puzzleModels))
