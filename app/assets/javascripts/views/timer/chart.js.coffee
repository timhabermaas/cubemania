class Cubemania.Views.Chart extends Cubemania.BaseView
  template: JST["timer/chart"]

  initialize: ->
    @bindTo @collection, "reset", @render, this
    @bindTo @collection, "add", @addSingleToChart, this
    @bindTo @collection, "remove", @removeSingleFromChart, this
    @bindTo @collection, "change", @updateSingleOnChart, this

  render: ->
    $(@el).html(@template())

    @chart = new Highcharts.Chart(
      chart:
        renderTo: @$("#chart")[0]
        type: "scatter"
      rangeSelector:
        selected: 1
      title:
        text: Cubemania.currentPuzzle.getName()
      xAxis:
        title:
          text: "Singles"
      yAxis:
        title:
          text: "Time"
        labels:
          formatter: ->
            formatTime(this.value)
      series: [
        id: Cubemania.currentUser.get("id")
        name: Cubemania.currentUser.get("name")
        data: (@chartDataFromSingle(s) for s in @collection.models)
      ]
    )
    this

  chartDataFromSingle: (single) ->
    {
      id: single.cid
      y: single.get("time")
      fillColor: if single.dnf() then "rgba(69, 114, 167, 0.5)" else "rgba(69, 114, 167, 1)"
    }

  addSingleToChart: (single) ->
    @chart.series[0].addPoint(@chartDataFromSingle(single))

  removeSingleFromChart: (single) ->
    @findPoint(single).remove()

  updateSingleOnChart: (single) ->
    @findPoint(single).update @chartDataFromSingle(single)

  findPoint: (single) ->
    _.find(@chart.series[0].data, (s) -> s.id == single.cid)

  onDispose: ->
    @chart.destroy()
