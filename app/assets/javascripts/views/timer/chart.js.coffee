class Cubemania.Views.Chart extends Cubemania.BaseView
  template: JST["timer/chart"]

  initialize: ->
    @bindTo @collection, "reset", @render, this
    @bindTo @collection, "add", @addSingleToChart, this
    @bindTo @collection, "remove", @removeSingleFromChart, this

  render: ->
    $(@el).html(@template())

    @chart = new Highcharts.Chart(
      chart:
        renderTo: @$("#chart")[0]
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
        data: _.map(@collection.models, (s) -> {id: s.cid, y: s.get("time")})
      ]
    )
    this

  addSingleToChart: (single) ->
    @chart.series[0].addPoint(id: single.cid, y: single.get("time"))

  removeSingleFromChart: (single) ->
    p = _.find(@chart.series[0].data, (s) -> s.id == single.cid)
    p.remove()

  onDispose: ->
    @chart.destroy()
