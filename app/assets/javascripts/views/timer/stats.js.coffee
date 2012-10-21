class Cubemania.Views.Stats extends Cubemania.BaseView
  template: JST["timer/stats"]
  detailTemplate: JST["timer/average_detail"]

  tagName: "section"
  id: "stats"

  events:
    "click .avg5 .details": "avg5Details"
    "click .avg12 .details": "avg12Details"

  initialize: (options) ->
    @singles = options.singles
    @bindTo @singles, "reset", @render, this
    @bindTo @singles, "change", @render, this
    @bindTo @singles, "add", @render, this
    @bindTo @singles, "destroy", @render, this # TODO use extra backbone view for singles/records

    @records = options.records
    @bindTo @records, "reset", @render, this

  render: ->
    avg5 = @singles.currentAverage(5)
    avg12 = @singles.currentAverage(12)

    records = for i in [1, 5, 12] # TODO move to records collection?
      @records.getByAmount(i)

    $(@el).html(@template(avg5: avg5, avg12: avg12, records: records))
    this

  avg5Details: (event) ->
    event.preventDefault()

    html = @detailTemplate(singles: @singles.last(5))
    $.fancybox({content: html})


  avg12Details: (event) ->
    event.preventDefault()

    html = @detailTemplate(singles: @singles.last(12))
    $.fancybox({content: html})
