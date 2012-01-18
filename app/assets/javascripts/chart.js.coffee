root = exports ? this

jQuery ->
  if $("#chart").length
    $.getJSON $("#chart").data("url"), (data) ->
      window.chart = new Highcharts.Chart(
        chart:
          renderTo: "chart"
          type: "spline"
        rangeSelector:
          selected: 1
        title:
          text: "huhu"
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
          data: data
        ]
      )
