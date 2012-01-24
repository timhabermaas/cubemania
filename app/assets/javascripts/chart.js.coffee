root = exports ? this

jQuery ->
  if $("#chart").length
    $("#singles li a.delete").live "click", (event) ->
      id = $(this).parent().parent().parent().data("id")
      element = chart.get(id)
      if element
        element.remove()

    $.getJSON $("#chart").data("url"), (singles) ->
      window.chart = new Highcharts.Chart(
        chart:
          renderTo: "chart"
        rangeSelector:
          selected: 1
        title:
          text: $("#chart").data("puzzle-name")
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
          name: $("#chart").data("user-name")
          data: singles
        ]
      )
