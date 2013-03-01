class Cubemania.Views.Stats extends Backbone.View
  template: JST["timer/stats"]
  detailTemplate: JST["timer/average_detail"]

  tagName: "section"
  id: "stats"

  events:
    "click .current .details": "currentAverageDetails"

  initialize: (options) ->
    @singles = options.singles
    @singles.on "reset", @render, this
    @singles.on "change", @render, this
    @singles.on "add", @render, this
    @singles.on "destroy", @render, this # TODO use extra backbone view for singles/records

    @records = options.records
    @records.on "reset", @render, this

  render: ->
    currentAverages = {}
    currentAverages[5] = @singles.currentAverage(5)
    currentAverages[12] = @singles.currentAverage(12)
    currentAverages[100] = @singles.currentMean(100)

    userSlug = Cubemania.currentUser.get("slug")

    $(@el).html(@template(currentAverages: currentAverages, records: @records.models, userSlug: userSlug))
    this

  currentAverageDetails: (event) ->
    event.preventDefault()
    count = $(event.currentTarget).data("count")

    html = @detailTemplate(singles: @singles.last(count))
    $.fancybox({content: html})
