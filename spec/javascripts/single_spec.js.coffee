describe "Single", ->
  describe "validations", ->
    it "should be valid if time is set", ->
      s = new Cubemania.Models.Single({time: 412})
      expect(s.isValid()).toEqual(true)

    it "should be invalid if time is not set", ->
      s = new Cubemania.Models.Single({})
      expect(s.isValid()).toEqual(false)

    it "should be invalid if time is NaN", ->
      s = new Cubemania.Models.Single({time: NaN})
      expect(s.isValid()).toEqual(false)

  describe "human_time", ->
    it "should set time to 73220 for 1:12.32", ->
      s = new Cubemania.Models.Single({human_time: "1:12.32"})
      expect(s.get("time")).toEqual(72320)

    it "should not update time if human_time isn't set", ->
      s = new Cubemania.Models.Single({time: 12})
      expect(s.get("time")).toEqual(12)
