$ ->
  $("#user_tokens").tokenInput "/users.json", {
    crossDomain: false
    theme: "facebook"
    preventDuplicates: true
    prePopulate: [{id: $("#singles").data("id"), name: $("#singles").data("name")}]
    hintText: "Type a User Name"
    onAdd: (item) ->
      $.getJSON "/users/" + item.id + "/puzzles/" + $("#singles").data("puzzle") + "/singles", (data) ->
        singles = (item.single.time for item in data)
        chart.addSeries {
          name: item.name
          id: item.id
          data: singles
        }
    onDelete: (item) ->
      chart.get(item.id).remove()
  }
