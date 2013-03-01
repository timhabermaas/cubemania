class Cubemania.Models.Record extends Backbone.Model

  containsSingle: (single) ->
    _.any(@get("singles"), (s) -> s.id == single.get("id"))

  getHtmlUrl: (slug) ->
    "/users/#{slug}/records/#{@get("id")}"

  title: -> # TODO move to RecordPresenter?
    @get("type_full_name")

  capitalizedTitle: ->
    @title()[0].toUpperCase() + @title()[1..-1]
