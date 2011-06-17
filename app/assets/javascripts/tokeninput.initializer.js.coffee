$ ->
  $("#user_tokens").tokenInput "/users.json", {
    crossDomain: false
    theme: "facebook"
    preventDuplicates: true
    prePopulate: [{id: $("#timer").data("user-id"), name: $("#timer").data("name")}]
    hintText: "Type a User Name"
    onResult: (results) ->
      #({id: item.user.id, name: item.user.name} for index, item in data)
      $.each results, (index, value) ->
        value.id = value.user.id
        value.name = value.user.name
      results
    onAdd: (item) ->
      addUserToChart item.id, item.name, $("#timer").data("puzzle-id")
    onDelete: (item) ->
      chart.get(item.id).remove()
  }