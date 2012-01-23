root = exports ? this

jQuery ->
  if $("#chart").length
    $.getJSON $("#chart").data("url"), (singles) ->
      singles = (single.time for single in singles)
      window.chart = new Highcharts.Chart(
        chart:
          renderTo: "chart"
        rangeSelector:
          selected: 1
        title:
          text: $("#chart").data("puzzle-name")
        xAxis:
          title:
            text: "Date"
          type: "datetime"
        yAxis:
          title:
            text: "Time"
          labels:
            formatter: ->
              formatTime(this.value)
        series: [
          name: $("#chart").data("user-name")
          data: singles
        ]
      )
