root = exports ? this

jQuery ->
  if $("#chart").length
    root.addUserToChart = (id, name) ->
      url = "/puzzles/#{$('#chart').data('puzzle-id')}/timer/chart?user_id=#{id}"
      $.getJSON url, (singles) ->
        chart.addSeries
          id: id
          name: name
          data: singles

    root.removeUserFromChart = (id) ->
      chart.get(id).remove()
