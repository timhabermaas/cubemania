describe "Single", ->
  describe "human_time", ->
    it "should set time to 73220 for 1:12.32", ->
      s = new Cubemania.Models.Single({human_time: "1:12.32"})
      expect(s.get("time")).toEqual(72320)

    it "should not update time if human_time isn't set", ->
      s = new Cubemania.Models.Single({time: 12})
      expect(s.get("time")).toEqual(12)
