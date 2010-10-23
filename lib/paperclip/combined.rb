module Paperclip
  class Combined < Thumbnail
    def transformation_command
      super + [" -crop '50x50+50+0'"] +
        if @attachment.instance.kind.image.path
          [compose_command]
        else
          [""]
        end
    end

    def compose_command
      kind = @attachment.instance.kind
      " -gravity SouthEast \"#{kind.image.path}\" -compose Over -composite"
    end
  end
end