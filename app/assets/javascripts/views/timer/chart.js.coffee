class Cubemania.Views.Chart extends Cubemania.BaseView
  template: JST["timer/chart"]

  events:
    "click a.by-solve": "bySolve"
    "click a.by-date": "byDate"

  @COLORS: [
    'rgba(223, 83, 83, 0.8)', # red
    'rgba(81, 115, 151, 0.85)' # blue
  ]

  byDate: (event) ->
    event.preventDefault()
    @$("p.tabs a").removeClass("selected")
    $(event.currentTarget).addClass("selected")

    if @singleChart
      @createDateChart()
      @addCurrentUserToChart()
      @singleChart = false
      @$("p.help").show()
      @$("ul.token-input-list-facebook").show()

  bySolve: (event) ->
    event.preventDefault()
    @$("p.tabs a").removeClass("selected")
    $(event.currentTarget).addClass("selected")

    if !@singleChart
      @createSinglesChart()
      @singleChart = true
      @$("p.help").hide()
      @$("ul.token-input-list-facebook").hide()

  initialize: ->
    @bindTo @collection, "reset", @render, this # TODO get rid of BaseView boilerplate
    @bindTo @collection, "reset", @createSinglesChart, this
    @bindTo @collection, "remove", @removeSingleFromChart, this
    @bindTo @collection, "change", @updateSingleInChart, this
    @singleChart = true

  # TODO extract common chart properties
  createSinglesChart: ->
    data = _.map @collection.models, @dataPointFromSingle

    @chart = new Highcharts.Chart(
      chart:
        renderTo: @$("#chart")[0]
        type: "scatter"
      title:
        text: Cubemania.currentPuzzle.getFullName()
      tooltip:
        formatter: ->
          t = "#{formatDateTime(this.point.date)}<br/>Time: <b>#{formatTime(this.y)}</b>"
          if this.point.comment?
            t += "<br/><i>#{formatScramble(this.point.comment)}</i>"
          t
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
      yAxis:
        title:
          text: "Time"
        labels:
          formatter: ->
            formatTime(this.value)
      series: [{
        name: Cubemania.currentUser.get("name")
        data: data
        color: Cubemania.Views.Chart.COLORS[0]
      }]
    )

  dataPointFromSingle: (single) ->
    {
      id: single.get("id")
      y: single.get("time")
      date: single.get("created_at")
      comment: single.get("comment")
    }

  addSingleToChart: (single) ->
    return unless @singleChart

    @chart.series[0].addPoint(@dataPointFromSingle(single))

  removeSingleFromChart: (single) ->
    return unless @singleChart
    @chart.get(single.get("id")).remove()

  updateSingleInChart: (single) ->
    return unless @singleChart

    point = @chart.get(single.get("id"))

    if point?
      point.update @dataPointFromSingle(single)
    else
      @addSingleToChart(single)

  createDateChart: ->
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
          "#{formatDate(this.x)}<br/>Mean: <b>#{formatTime(this.y)}</b>"
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
        events:
          setExtremes: (event) => # TODO have one big chart (monthly cached, which gets set again when user zooms out)
            userIds = @userIdsInChart()
            if event.min?
              @updateDataForUsers userIds, event.min / 1000, event.max / 1000
            else
              @updateDataForUsers userIds, null, null
      yAxis:
        title:
          text: "Time"
        labels:
          formatter: ->
            formatTime(this.value)
      series: []
    )

  render: ->
    $(@el).html(@template())
    @$("p.help").hide()

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

    @$("ul.token-input-list-facebook").hide()

    this

  subtitle: (data = []) ->
    if data.length > 0
      from = formatDate(new Date(data[data.length - 1].x))
      to = formatDate(new Date(data[0].x))
      "from #{from} to #{to}"
    else
      "from ? to ?"

  addCurrentUserToChart: ->
    @addUserToChart Cubemania.currentUser.get("id"), Cubemania.currentUser.get("name")

  # TODO add convenient function for zooming on current day
  fetchDataForChart: (userId, from, to, callback) ->
    puzzleId = Cubemania.currentPuzzle.getId()
    $.getJSON "/api/puzzles/#{puzzleId}/singles/chart.json?from=#{from}&to=#{to}&user_id=#{userId}", (data) =>
      callback(@generateChartDataFromApiData data)

  addUserToChart: (id, name, color = Cubemania.Views.Chart.COLORS[0]) ->
    @fetchDataForChart id, null, null, (data) =>
      @chart.addSeries
        id: id
        name: name
        color: color
        data: data
      @chart.setTitle({}, { text: @subtitle(data) }) # TODO duplication

  updateDataForUser: (id, from, to) ->
    @fetchDataForChart id, from, to, (data) =>
      @chart.get(id).setData(data)
      @chart.setTitle({}, { text: @subtitle(data) }) # TODO set tooltip according to groupBy

  updateDataForUsers: (ids, from, to) ->
    @updateDataForUser(id, from, to) for id in ids

  generateChartDataFromApiData: (apiData) ->
    _.map apiData, (single) ->
      {
        x: single.created_at_timestamp * 1000
        y: single.time
      }

  userIdsInChart: ->
    ids = _.pluck @$("#user-tokens").tokenInput("get"), "id"
    ids.concat [Cubemania.currentUser.get("id")]

  removeUserFromChart: (id) ->
    @chart.get(id).remove()

  onDispose: ->
    @chart.destroy() if @chart?
