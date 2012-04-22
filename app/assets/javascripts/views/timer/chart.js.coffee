class Cubemania.Views.Chart extends Backbone.View
  template: JST["timer/chart"]

  initialize: ->
    @collection.on "reset", @render, this
    @collection.on "add", @addSingleToChart, this
    @collection.on "remove", @removeSingleFromChart, this

  render: ->
    $(@el).html(@template())

    @chart = new Highcharts.Chart(
      chart:
        renderTo: @$("#chart")[0]
      rangeSelector:
        selected: 1
      title:
        text: Cubemania.currentPuzzle.name
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
        data: _.map(@collection.models, (s) -> {y: s.get("time"), id: s.get("id")})
      ]
    )
    this

  addSingleToChart: (single) ->
    @chart.series[0].addPoint(id: single.get("id"), y: single.get("time"))

  removeSingleFromChart: (single) ->
    p = _.find(@chart.series[0].data, (s) -> s.id == single.id)
    p.remove()

