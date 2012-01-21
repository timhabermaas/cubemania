$.extend($.gritter.options,
  fade_in_speed: "slow"
  fade_out_speed: "slow"
  time: 15000
)


$(document).ajaxComplete (e, request, options) ->
  message = request.getResponseHeader("X-Message-Content")
  title = request.getResponseHeader("X-Message-Title")
  image = request.getResponseHeader("X-Message-Image")
  imagePosition = request.getResponseHeader("X-Message-Image-Position")

  if message
    messages = message.split("@")
    titles = title.split("@")
    $.each messages, (index, item) ->
      $.gritter.add {
        title: titles[index]
        text: item
        image: image
      }
      if imagePosition
        $(".gritter-image").css("background-position", "-#{imagePosition * 50}px 0px")
