class Cubemania.Views.Chart extends Cubemania.BaseView
  template: JST["timer/chart"]

  @COLORS: [
    'rgba(223, 83, 83, 0.8)', # red
    'rgba(81, 115, 151, 0.8)' # blue
  ]

  # TODO zoom
  # TODO duplicated code
  switchToDay: =>
    @switchTo "day"

  switchToWeek: =>
    @switchTo "week"

  switchToMonth: =>
    @switchTo "month"

  switchTo: (interval) ->
    @groupBy = interval
    @removeAllData()
    @addCurrentUserToChart()

  removeAllData: ->
    @$("#user-tokens").tokenInput "clear"
    @$("#user-tokens").blur()
    @chart.series[0].remove()

  initialize: ->
    @groupBy = "week"
    @tabs = @addSubview new Cubemania.Views.Tabs
      title: "Group by:"
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

  render: ->
    $(@el).html(@template())
    @$("#chart").after(@tabs.render().el)

    @chart = new Highcharts.Chart(
      chart:
        renderTo: @$("#chart")[0]
        type: "scatter"
        zoomType: "x"
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
            radius: 7
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
        @addUserToChart item.id, item.name, Cubemania.Views.Chart.COLORS[1] # TODO cycle colors
      onDelete: (item) =>
        @removeUserFromChart(item.id)

    if Cubemania.currentUser.present()
      @addCurrentUserToChart()
    else
      $(@el).html("<p class='suggestion'>You're currently not logged in!<br /> <a href='/login'>Login</a> or <a href='/register'>register</a> to save your times permanently. </p>")

    this

  subtitle: (data = []) ->
    if data.length > 0
      from = formatDate(new Date(data[data.length - 1][0]))
      to = formatDate(new Date(data[0][0]))
      "from #{from} to #{to}"
    else
      "from ? to ?"

  addCurrentUserToChart: ->
    @addUserToChart Cubemania.currentUser.get("id"), Cubemania.currentUser.get("name")

  addUserToChart: (id, name, color = Cubemania.Views.Chart.COLORS[0]) ->
    puzzleId = Cubemania.currentPuzzle.puzzle.get("id")
    $.getJSON "/api/puzzles/#{puzzleId}/singles/grouped.json?by=#{@groupBy}&user_id=#{id}", (data) =>
      data = @generateChartDataFromApiData data
      @chart.addSeries
        id: id
        name: name
        color: color
        data: data
      @chart.setTitle({}, { text: @subtitle(data) })
      # TODO set tooltip according to groupBy

  generateChartDataFromApiData: (apiData) ->
    _.map apiData, (single) -> [single.created_at_timestamp * 1000, single.time]

  removeUserFromChart: (id) ->
    @chart.get(id).remove()

  onDispose: ->
    @chart.destroy()
