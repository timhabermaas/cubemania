Array::shuffle = -> @sort -> 0.5 - Math.random()

Array::sample = -> this.shuffle()[0]

Array::remove = (v) -> x for x in @ when x!=v
