class Cubemania.Views.Stats extends Cubemania.BaseView
  template: JST["timer/stats"]
  detailTemplate: JST["timer/average_detail"]

  tagName: "section"
  id: "stats"

  currentAverageCounts: [5, 12, 100]

  events:
    "click .current .details": "currentAverageDetails"

  initialize: (options) ->
    @singles = options.singles
    @bindTo @singles, "reset", @render, this
    @bindTo @singles, "change", @render, this
    @bindTo @singles, "add", @render, this
    @bindTo @singles, "destroy", @render, this # TODO use extra backbone view for singles/records

    @records = options.records
    @bindTo @records, "reset", @render, this

  render: ->
    currentAverages = {}
    currentAverages[n] = @singles.currentAverage(n) for n in @currentAverageCounts

    records = for i in [1, 5, 12] # TODO move to records collection?
      @records.getByAmount(i)

    userSlug = Cubemania.currentUser.get("slug")

    $(@el).html(@template(currentAverages: currentAverages, records: records, userSlug: userSlug))
    this

  currentAverageDetails: (event) ->
    event.preventDefault()
    count = $(event.currentTarget).data("count")

    html = @detailTemplate(singles: @singles.last(count))
    $.fancybox({content: html})
