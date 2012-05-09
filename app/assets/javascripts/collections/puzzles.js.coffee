class Cubemania.Collections.Puzzles extends Backbone.Collection
  model: Cubemania.Models.Puzzle

  findByIdOrSlug: (idOrSlug) ->
    _.find(@models, (p) -> p.get("slug") == idOrSlug || !isNaN(idOrSlug) && p.get("id") == parseInt(idOrSlug))
