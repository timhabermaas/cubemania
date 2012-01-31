jQuery ->
  $("#user-tokens").tokenInput "/users.json", {
    crossDomain: false
    theme: "facebook"
    preventDuplicates: true
    prePopulate: [{id: $("#chart").data("user-id"), name: $("#chart").data("user-name")}]
    hintText: "Compare with..."
    onAdd: (item) ->
      addUserToChart item.id, item.name
    onDelete: (item) ->
      removeUserFromChart(item.id)
  }
