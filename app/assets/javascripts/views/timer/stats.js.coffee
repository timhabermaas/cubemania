class Cubemania.Views.Stats extends Backbone.View
  template: JST["timer/stats"]

  tagName: "section"
  id: "stats"

  initialize: (options) ->
    @singles = options["singles"]
    @singles.on("change", @render, this) # TODO use extra backbone view for singles/records
    @records = options["records"]
    @records.on("change", @render, this)

  render: ->
    console.log "render stats"
    $(@el).html(@template())
    this