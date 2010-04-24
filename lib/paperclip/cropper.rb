module Paperclip
  class Cropper < Thumbnail
    def transformation_command
      crop_command + super.sub(/ -crop \S+/, '')
    end

    def crop_command
      " -crop '50x50+50+0'"
    end
  end
end