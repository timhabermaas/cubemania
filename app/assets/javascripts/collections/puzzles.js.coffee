class Cubemania.Collections.Puzzles extends Backbone.Collection
  model: Cubemania.Models.Puzzle

  findByIdOrSlug: (idOrSlug) ->
    @where(id: idOrSlug)[0] || @where(slug: idOrSlug)[0]
