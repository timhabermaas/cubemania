root = exports ? this

$ ->
  if $("#chart").length > 0
    root.chart = new Highcharts.Chart {
      chart:
        renderTo: "chart"
      title:
        text: "Cubing Progress"
      xAxis:
        title:
          text: "Solves"
        allowDecimals: false
        labels:
          formatter: ->
            this.value + 1
      yAxis:
        title:
          text: "Time"
        labels:
          formatter: ->
            formatTime(this.value)
      tooltip:
        formatter: ->
          "hey :)"
      plotOptions:
        line:
          enableMouseTracking: true
          allowPointSelect: true
      #series: [{}]
    }
    addUserToChart($("#timer").data("user-id"), $("#timer").data("name"), $("#timer").data("puzzle-id"))

root.addUserToChart = (userId, userName, puzzleId) ->
  $.getJSON "/users/" + userId + "/puzzles/" + puzzleId + "/singles", (data) ->
    singles = (single.single.time for single in data)
    chart.addSeries {
      name: userName
      id: userId
      data: singles
    }