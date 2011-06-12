$ ->
  $("#user_tokens").tokenInput "/users.json", {
    crossDomain: false
    theme: "facebook"
    preventDuplicates: true
    prePopulate: [{id: $("#singles").data("id"), name: $("#singles").data("name")}]
    hintText: "Type a User Name"
    onResult: (results) ->
      #({id: item.user.id, name: item.user.name} for index, item in data)
      $.each results, (index, value) ->
        value.id = value.user.id
        value.name = value.user.name
      results
    onAdd: (item) ->
      $.getJSON "/users/" + item.id + "/puzzles/" + $("#singles").data("puzzle") + "/singles", (data) ->
        singles = (single.single.time for single in data)
        chart.addSeries {
          name: item.name
          id: item.id
          data: singles
        }
    onDelete: (item) ->
      chart.get(item.id).remove()
  }