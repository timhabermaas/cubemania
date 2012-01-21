$(document).ajaxComplete (e, request, options) ->
  message = request.getResponseHeader("X-Message-Content")
  title = request.getResponseHeader("X-Message-Title")
  image = request.getResponseHeader("X-Message-Image")
  if message?
    messages = message.split("@")
    titles = title.split("@")
    $.each messages, (index, item) ->
      $.gritter.add {
        title: titles[index]
        text: item
        image: image
        time: 15000
      }
