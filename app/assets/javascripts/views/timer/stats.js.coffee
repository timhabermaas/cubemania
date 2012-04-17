class Cubemania.Views.Stats extends Backbone.View
  template: JST["timer/stats"]

  tagName: "section"
  id: "stats"

  initialize: (options) ->
    @singles = options["singles"]
    @singles.on("change", @render, this)
    @singles.on("add", @render, this)
    @singles.on("destroy", @render, this) # TODO use extra backbone view for singles/records
    @records = options["records"]
    @records.on("change", @render, this)

  render: ->
    avg5 = @singles.currentAverage(5)
    avg12 = @singles.currentAverage(12)
    $(@el).html(@template(avg5: avg5, avg12: avg12))
    this
