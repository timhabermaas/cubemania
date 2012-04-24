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
        data: @chartDataFromSingle(s, true) for s in @collection.models
      ]
    )

    @$("#user-tokens").tokenInput "/users.json",
      crossDomain: false
      theme: "facebook"
      preventDuplicates: true
      onResult: (results) ->
        _.filter(results, (r) -> r.id != Cubemania.currentUser.get("id")) # remove self from list
      hintText: "Compare with..."
      onAdd: (item) =>
        @addUserToChart item.id, item.name
      onDelete: (item) =>
        @removeUserFromChart(item.id)

    this

  chartDataFromSingle: (single, setColor = false) -> # TODO make color setable / cycle color
    if setColor
      {
        id: single.cid
        y: single.get("time")
        fillColor: if single.dnf() then "rgba(69, 114, 167, 0.5)" else "rgba(69, 114, 167, 1)"
      }
    else
      {
        id: single.cid
        y: single.get("time")
      }

  addSingleToChart: (single) ->
    @chart.series[0].addPoint(@chartDataFromSingle(single, true))

  removeSingleFromChart: (single) ->
    @findPoint(single).remove()

  updateSingleOnChart: (single) ->
    @findPoint(single).update @chartDataFromSingle(single, true)

  addUserToChart: (id, name) ->
    singles = new Cubemania.Collections.Singles(Cubemania.currentPuzzle.puzzle.get("id"))
    singles.fetch({data: {user_id: id}, async: false})
    @chart.addSeries
      id: id
      name: name
      data: @chartDataFromSingle(s) for s in singles.models

  removeUserFromChart: (id) ->
    @chart.get(id).remove()

  findPoint: (single) ->
    _.find(@chart.series[0].data, (s) -> s.id == single.cid)

  onDispose: ->
    @chart.destroy()
