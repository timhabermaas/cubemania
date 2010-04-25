module Paperclip
  class Facebook < Thumbnail
    def transformation_command
      super + " -crop '50x50+50+0'" +
        if @attachment.instance.kind.image.path
          compose_command
        else
          ""
        end
    end

    def compose_command
      kind = @attachment.instance.kind
      kind_file = Tempfile.new("kind")
      kind_file.write(AWS::S3::S3Object.value(kind.image.path, @attachment.bucket_name))
      kind_file.rewind
      " -gravity SouthEast \"#{kind_file.path}\" -compose Over -composite"
    rescue AWS::S3::NoSuchKey
      Paperclip.log "AWS::S3::NoSuchKey error"
      ""
    end
  end
end