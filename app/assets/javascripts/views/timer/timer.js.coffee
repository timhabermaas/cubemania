class Cubemania.Views.Timer extends Backbone.View
  template: JST["timer/timer"]

  events:
    "click a.toggle": "toggleManual"
    "submit #new_single": "submitSingle"
    "touchstart .time-container": "keyDown"
    "touchend .time-container": "keyUp"
    "submit #add_comment": "addComment"
    "click a.add_comment": "toggleComment"
    "click input.inspection-toggle": "toggleInspection"
    "focus div.add_comment textarea": "disableTimer"
    "blur div.add_comment textarea": "enableTimer"

  initialize: ->
    @timer = new Cubemania.TimerWithInspection(0)
    @timer.on "stopped", @displayAddCommentBubble, this
    @timer.on "stopped", @createSingle, this
    @timer.on "started", @hideStuff, this
    @timer.on "isAboutToStart", @makeTimeGreen, this
    @timer.on "countdownStarted", @hideStuff, this
    @timerEnabled = true
    @scramble = Cubemania.scrambler.scramble(Cubemania.currentPuzzle.getName())
    $(document).keydown(@keyDown)
    $(document).keyup(@keyUp)
    setInterval(@updateDisplay, 41)

  updateDisplay: =>
    if @timer.isCountdownRunning()
      @$(".time").html(Math.ceil(@timer.currentTime() / 1000))
    else
      @$(".time").html(formatTime(@timer.currentTime()))

  updateScramble: ->
    @scramble = Cubemania.scrambler.scramble(Cubemania.currentPuzzle.getName())
    @$(".scramble").html(formatScramble @scramble)

  render: ->
    $(@el).html(@template(scramble: @scramble, timer: @timer))
    this

  keyUp: (event) =>
    if (event.type == "touchend" or event.keyCode == 32) and @timerEnabled
      event.preventDefault()
      @timer.wantToStart()
      @$(".time").removeClass("starting")

  keyDown: (event) =>
    if (event.type == "touchstart" or event.keyCode == 32) and @timerEnabled
      event.preventDefault()
      @timer.wantToStop()

  toggleManual: (event) ->
    event.preventDefault()
    @$("#new_single").toggle()
    @$(".time-container").toggle()
    @timerEnabled = !@timerEnabled
    ct = event.currentTarget
    @$(ct).toggleText("Changed your mind?", "Set times manually")
    if @$(ct).text() == "Changed your mind?"
      @$("#single_human_time").focus()
    else
      @$("#single_human_time").blur()

  toggleComment: (event) -> # TODO confusion between toggle and hide
    event.preventDefault() if event?
    @$("div.add_comment form").toggle()
    @$("div.add_comment a").toggle()
    @$("div.add_comment form")[0].reset()

  toggleInspection: (event) ->
    inspection = if @$("input.inspection-toggle").prop("checked") then 15 else 0
    @timer.reset(inspection)
    @render()

  enableTimer: ->
    @timerEnabled = true

  disableTimer: ->
    @timerEnabled = false

  submitSingle: (event) =>
    event.preventDefault()
    @collection.create({human_time: @$("#single_human_time").val(), scramble: @scramble})
    @updateScramble()
    @$("form")[0].reset()

  createSingle: (time) ->
    @collection.create({time: @timer.currentTime(), scramble: @scramble})
    @updateScramble() # TODO duplication

  addComment: (event) ->
    event.preventDefault()
    lastSingle = @collection.models[@collection.length - 1]
    lastSingle.set "comment", @$("#add_comment [name='comment']").val()
    lastSingle.save()
    @toggleComment()
    @hideAddCommentBubble()

  makeTimeGreen: ->
    @$(".time").addClass("starting")

  displayAddCommentBubble: ->
    @$("div.add_comment").slideDown()

  hideAddCommentBubble: ->
    @$("div.add_comment").slideUp()
    @$("div.add_comment form").hide()
    @$("div.add_comment a").show()

  hideStuff: ->
    @hideAddCommentBubble()
    Cubemania.flashView.slideUp()
