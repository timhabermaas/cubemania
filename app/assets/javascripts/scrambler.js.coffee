class Cubemania.Scrambler
  scramble: (puzzle) ->
    switch puzzle.toLowerCase()
      when "2x2x2" then cube [["R", "L"], ["F", "B"], ["U", "D"]], 25 # TODO get scramble length from actual puzzle?
      when "3x3x3" then cube [["R", "L"], ["F", "B"], ["U", "D"]], 25
      when "4x4x4" then cube [["R", "L", "Rw", "Lw"], ["F", "B", "Fw", "Bw"], ["D", "U", "Dw", "Uw"]], 40
      when "5x5x5" then cube [["R", "L", "Rw", "Lw"], ["F", "B", "Fw", "Bw"], ["D", "U", "Dw", "Uw"]], 60
      when "6x6x6" then cube [["R", "L", "2R", "2L", "3R", "3L"], ["F", "B", "2F", "2B", "3F", "3B"], ["D", "U", "2D", "2U", "3D", "3U"]], 80
      when "7x7x7" then cube [["R", "L", "2R", "2L", "3R", "3L"], ["F", "B", "2F", "2B", "3F", "3B"], ["D", "U", "2D", "2U", "3D", "3U"]], 100
      when "megaminx" then megaminx(7, 10)
      when "pyraminx" then pyraminx(25)
      when "clock" then clock(25)
      when "square-1" then square1()
      else ""

  cube = (turns, length) ->
    variants = ['', "'", '2']
    axis = rand turns.length
    (
      (
        axis = (axis + rand(turns.length - 1) + 1) % turns.length
        turns[axis].sample() + variants.sample()
      ) for x in [1..length]
    ).join " "

  megaminx = (lines, columns) ->
    turns = ["R", "D"]
    variants = ["--", "++"]
    variantsForU = ["'", ""]

    ((
      row = (turns[column % 2] + variants.sample() for column in [0..(columns - 1)])
      row.push("U#{variantsForU.sample()}")
      row.join " "
    ) for line in [1..lines]).join "\n"

  pyraminx = (length) ->
    turns = ["U", "L", "R", "B"]
    variants = ['', "'"]

    tipLength = rand 4
    tipTurns = (t.toLowerCase() for t in turns).shuffle()[0..tipLength]

    scramble = (t + variants.sample() for t in tipTurns)

    axis = rand turns.length
    scramble = scramble.concat(
      (
        axis = (axis + rand(turns.length - 1) + 1) % turns.length
        turns[axis] + variants.sample() # TODO extract duplication
      ) for x in [(tipLength + 1)..length]
    )
    scramble.join " "

  clock = (length) ->
    pins = ["U", "d"]
    states = ["UUdd", "dUdU", "ddUU", "UdUd", "dUUU", "UdUU", "UUUd", "UUdU", "UUUU", "dddd"]

    scramble = ((
      moves = []
      u = rand(12) - 5
      d = rand(12) - 5
      moves.push "u=" + u if state.replace("d", "").length > 1
      moves.push "d=" + d if state.replace("U", "").length > 1
      state + " " + moves.join "; "
    ) for state in states)

    scramble.push (pins.sample() for i in [1..4]).join ""
    scramble.join " / "

  square1 = ->
    scramble = []
    upLayer = ((if i % 2 == 0 then 30 else 60) for i in [0..7])
    downLayer = ((if i % 2 == 0 then 30 else 60) for i in [0..7]) # TODO clone
    length = 0
    while length <= 40
      upMoves = possibleMoves upLayer
      downMoves = possibleMoves downLayer
      upMove = upMoves.sample()
      downMoves = downMoves.remove 0 if upMove == 0
      downMove = downMoves.sample()
      scramble.push [humanize_sq_one_move(upLayer, upMove), humanize_sq_one_move(downLayer, downMove) * -1]
      upLayer = doMove upLayer, upMove
      downLayer = doMove downLayer, downMove
      length += if upMove == 0 then 0 else 1
      length += if downMove == 0 then 0 else 1
      temp = doSlice(upLayer, downLayer)
      upLayer = temp.up
      downLayer = temp.down
      length += 1

    ("(#{s.join(', ')})" for s in scramble).join " / "

  possibleMoves = (layer) ->
    result = []

    ((
      sum = 0
      if _.any(layer, (e, i) -> sum += layer[(start + i) % layer.length]; sum == 180)
        result.push start
    ) for e, start in layer)

    result

  doMove = (layer, move) ->
    move = move % layer.length
    if move == 0
      layer
    else
      layer[move..-1].concat layer[0..move-1]

  splitLayer = (layer) ->
    sum = 0
    small = (e for e in layer when (sum += e; sum <= 180))
    big = layer[small.length..-1]
    [small, big]

  doSlice = (up, down) ->
    upSplits = splitLayer up
    downSplits = splitLayer down
    return {up: downSplits[0].reverse().concat(upSplits[1]), down: upSplits[0].reverse().concat(downSplits[1])}

  humanize_sq_one_move = (layer, move) ->
    if move != 0
      move = _.reduce(layer[0..move - 1], ((sum, x) -> if x == 30 then sum + 1 else sum + 2), 0)
    if move > 6
      move - 12
    else
      move

  rand = (n) ->
    Math.floor(Math.random() * n)
