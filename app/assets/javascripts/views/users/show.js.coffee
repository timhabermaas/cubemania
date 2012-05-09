class Cubemania.Views.UsersShow extends Cubemania.BaseView

  template: JST["users/show"]
  id: "user"

  events:
    "click .tabs a": "changeAverageSize"

  initialize: ->
    @bindTo @model, "change", @render, this

  wcaLink: ->
    '<a href="http://www.worldcubeassociation.org/results/p.php?i=' + @model.get('wca') + '">' + @model.get("name") + '\'s World Cube Association Profile</a>'

  changeAverageSize: (event) ->
    event.preventDefault()
    @$(".tabs a").removeClass("selected")
    target = $(event.currentTarget)
    target.addClass("selected")

    if target.hasClass("avg5")
      @$("td.avg5").show()
      @$("td.avg12").hide()
    else
      @$("td.avg5").hide()
      @$("td.avg12").show()

  render: ->
    records = _.groupBy @model.records.models, (r) -> r.get("puzzle_id")
    records = for key, value of records
      {
        single: _.find(value, (r) -> r.get("amount") == 1),
        avg5: _.find(value, (r) -> r.get("amount") == 5),
        avg12: _.find(value, (r) -> r.get("amount") == 12),
        puzzle: value[0].get("puzzle")
      } # TODO sort by name?

    timerPath = $("nav.main li:nth-child(2) a").attr("href")

    $(@el).html(@template(user: @model, wcaLink: @wcaLink(), records: records, timerPath: timerPath))
    @$("td.avg12").hide()
    this
