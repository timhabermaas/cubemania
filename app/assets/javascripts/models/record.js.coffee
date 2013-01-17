class Cubemania.Models.Record extends Backbone.Model

  containsSingle: (single) ->
    _.any(@get("singles"), (s) -> s.id == single.get("id"))

  getHtmlUrl: (slug) ->
    "/users/#{slug}/records/#{@get("id")}"

  title: -> # TODO move to RecordPresenter?
    if @get("amount") == 1
      "single"
    else
      "average of #{@get('amount')}"

  capitalizedTitle: ->
    @title()[0].toUpperCase() + @title()[1..-1]
