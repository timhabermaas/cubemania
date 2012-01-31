root = exports ? this

jQuery ->
  if $("#chart").length
    $("#singles li a.delete").live "click", (event) ->
      id = $(this).parent().parent().parent().data("id")
      element = chart.get(id)
      if element
        element.remove()
      event.preventDefault()

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
          id: $("#chart").data("user-id")
          name: $("#chart").data("user-name")
          data: singles
        ]
      )

    root.addUserToChart = (id, name) ->
      url = "/puzzles/#{$('#chart').data('puzzle-id')}/timer/chart?user_id=#{id}"
      $.getJSON url, (singles) ->
        chart.addSeries
          id: id
          name: name
          data: singles

    root.removeUserFromChart = (id) ->
      chart.get(id).remove()
