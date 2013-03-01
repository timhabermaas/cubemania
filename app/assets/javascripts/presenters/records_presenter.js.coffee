class Cubemania.Presenters.RecordsPresenter

  constructor: (records) ->
    @records = records

  flashMessage: (single) ->
    messages = for record in @records.models
      if record && record.containsSingle(single)
        "You have a new #{record.title()} record: <strong>#{formatTime(record.get("time"))}</strong>! <a href='#{record.getHtmlUrl(Cubemania.currentUser.get("slug"))}'>Share it!</a>"

    _.compact(messages).join("<br />")
