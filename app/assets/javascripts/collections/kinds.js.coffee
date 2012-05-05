class Cubemania.Collections.Kinds extends Backbone.Collection
  model: Cubemania.Models.Kind

  puzzles: ->
    puzzleModels = (((p.set("kind", k.attributes); p) for p in k.puzzles.models) for k in @models)
    new Cubemania.Collections.Puzzles(_.flatten(puzzleModels))
