class Cubemania.Presenters.RecordsPresenter

  constructor: (records) ->
    @records = records

  flashMessage: (single) ->
    records = _.map([1, 5, 12], (a) => @records.getByAmount(a))
    messages = for record in records
      if record && record.containsSingle(single)
        "You have a new #{record.title()} record: <strong>#{formatTime(record.get("time"))}</strong>! <a href='#{record.getHtmlUrl(Cubemania.currentUser.get("slug"))}'>Share it!</a>"

    _.compact(messages).join("<br />")
