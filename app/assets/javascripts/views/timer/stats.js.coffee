class Cubemania.Views.Stats extends Cubemania.BaseView
  template: JST["timer/stats"]

  tagName: "section"
  id: "stats"

  initialize: (options) ->
    @singles = options.singles
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
