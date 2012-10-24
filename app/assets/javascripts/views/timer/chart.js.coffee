class Cubemania.Views.Chart extends Cubemania.BaseView
  template: JST["timer/chart"]

  # TODO duplicated code
  switchToDay: =>
    @groupBy = "day"
    @removeAllData()
    @fetchData(Cubemania.currentPuzzle.puzzle.get("id"),
      Cubemania.currentUser.get("id"))

  switchToWeek: =>
    @groupBy = "week"
    @removeAllData()
    @fetchData(Cubemania.currentPuzzle.puzzle.get("id"),
      Cubemania.currentUser.get("id"))

  switchToMonth: =>
    @groupBy = "month"
    @removeAllData()
    @fetchData(Cubemania.currentPuzzle.puzzle.get("id"),
      Cubemania.currentUser.get("id"))

  removeAllData: ->
    @$("#user-tokens").tokenInput "clear"
    @$("#user-tokens").blur()
    @chart.series[0].remove()

  initialize: ->
    @groupBy = "week"
    @tabs = @addSubview new Cubemania.Views.Tabs
      title: "Group by: "
      tabs: [
        name: "Day"
        className: "day"
        callback: @switchToDay
      ,
        name: "Week"
        className: "week"
        callback: @switchToWeek
      ,
        name: "Month"
        className: "month"
        callback: @switchToMonth
      ]
      selectedIndex: 1

  fetchData: (puzzleId, userId) ->
    $.getJSON "/api/puzzles/#{puzzleId}/singles/grouped.json?by=#{@groupBy}&user_id=#{userId}", (data) =>
      data = _.map data, (single) -> [single.created_at_timestamp * 1000, single.time]
      @addDataToChart data

  render: ->
    $(@el).html(@template())
    @$("#chart").after(@tabs.render().el)

    @chart = new Highcharts.Chart(
      chart:
        renderTo: @$("#chart")[0]
        type: "scatter"
      title:
        text: "Your cubing progress in #{Cubemania.currentPuzzle.getFullName()}"
      subtitle:
        text: @subtitle()
      tooltip:
        formatter: ->
          "#{formatDate(this.x)}<br/>Mean: #{formatTime(this.y)}"
      plotOptions:
        scatter:
          marker:
            radius: 5
            symbol: "circle"
            states:
              hover:
                enabled: true
                lineColor: 'rgb(100,100,100)'
          states:
            hover:
              marker:
                enabled: false
      xAxis:
        type: "datetime"
      yAxis:
        title:
          text: "Time"
        labels:
          formatter: ->
            formatTime(this.value)
      series: []
    )

    @$("#user-tokens").tokenInput "/api/users.json",
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


    puzzleId = Cubemania.currentPuzzle.puzzle.get("id")
    userId = Cubemania.currentUser.get("id")
    @fetchData(puzzleId, userId)
    this

  chartDataFromSingle: (single, setColor = false) -> # TODO make color setable / cycle color
    if setColor
      {
        id: single.cid
        y: single.get("time")
        fillColor: if single.dnf() then "rgba(69, 114, 167, 0.5)" else "rgba(69, 114, 167, 1)"
        single: single # TODO memory?
      }
    else
      {
        id: single.cid
        y: single.get("time")
        single: single
      }

  subtitle: (data = []) ->
    if data.length > 0
      "from #{formatDate(new Date(data[data.length - 1][0]))} to today"
    else
      "from ? to today"

  addDataToChart: (data) ->
    @chart.addSeries
      id: Cubemania.currentUser.get("id")
      name: Cubemania.currentUser.get("name")
      color: 'rgba(223, 83, 83, 0.8)'
      data: data
    @chart.setTitle({}, {text: @subtitle(data)})

  addUserToChart: (id, name) ->
    singles = new Cubemania.Collections.Singles([], puzzleId: Cubemania.currentPuzzle.puzzle.get("id"))
    singles.fetch({data: {user_id: id}, async: false}) # TODO make asynchronous by and add singles on success callback
    @chart.addSeries
      id: id
      name: name
      data: @chartDataFromSingle(s) for s in singles.models

  removeUserFromChart: (id) ->
    @chart.get(id).remove()

  onDispose: ->
    @chart.destroy()
