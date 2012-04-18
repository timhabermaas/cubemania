Array::shuffle = -> @sort -> 0.5 - Math.random()

Array::sample = -> this.shuffle()[0]
