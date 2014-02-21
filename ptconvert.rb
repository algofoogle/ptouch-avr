#!/usr/bin/env ruby
require 'optparse'
require 'shellwords'

# NOTE: This was tested with Ruby 1.8.7 and MAY not work the same with Ruby 1.9.3.

# Process command-line options:
options = {}
OptionParser.new do |o|
  o.banner = <<EOH
ptconvert will take an image file and convert it to a binary of PT-1010 image
data that can be included directly into ptouchavr.asm.
Usage: ruby ptconvert.rb [options] INFILE [OUTFILE]
NOTE: OUTFILE defaults to image.* (ext. depending on format; .raw by default)
EOH
  o.on('-f', '--format FORMAT', [:raw, :pbm, :asm, :bp1, :bp2, :bp3, :bp4],
    'Output format. Options:',
    'raw (default), pbm, asm, bp1, bp2, bp3, bp4'
  ) do |v|
    options[:format] = v
  end
  o.on('-d', '--displace OFFSET', 'Offset the image on the pixel axis (for bp* formats only') do |v|
    options[:displace] = v
  end
end.parse!

# Get filename(s):
raise "INFILE argument not specified" if ARGV.empty?
raise "Too many arguments (#{ARGV.count})" if ARGV.count > 2
source_image = ARGV.shift

format = options[:format] || :raw

target_bin = ARGV.shift || "image.#{format.to_s}"

raise "Displacement can only be used for 'bp' formats" if options[:displace] && format.to_s !~ /^bp[0-9]$/

# Define a temporary target file:
tempfile = "_tmp_ptconvert_#{'%08i' % rand(100000000)}.pbm"

# Get ImageMagick's "convert" to convert the source image to our temporary file
# (an uncompressed PBM file):
`convert #{source_image.shellescape} -compress None #{tempfile.shellescape}`

def bp4_compare(buffer, line)
  comp = [buffer,line].transpose
  # Generate the bitfield by comparing pairs:
  bitfield = comp.map{|p| (p[0]==p[1]) ? 0 : 1}.join
  # Pack the bytes that are different:
  bytes = ''
  comp.each {|p| bytes << p[1] if p[0]!=p[1]}
  # Store this line as the new buffer:
  buffer = line
  # Return the compressed line:
  ([bitfield].pack('B*') << bytes)
end

size = nil
# Now try to process the raw PBM file:
begin
  # Read the file contents and split the data by whitespace:
  data = IO.read(tempfile).strip.split(/\s+/)
  # Verify the format:
  version = data.shift
  raise "Wrong file format: Expecting \"P1\", got #{version}" unless version == 'P1'
  # Convert the remaining data into integers:
  data.map!(&:to_i)
  # Get dimensions:
  width, height = data.shift(2)
  # Now break the stream of data into "lines":
  image = data.each_slice(width).to_a
  # Determine orientation:
  if width == 64
    # Image is already 64 pixels wide, so we assume it is oriented the way the
    # TPH needs it. We just need to flip the pixels of each line:
    image.map!(&:reverse)
  elsif height == 64
    # Height is 64 pixels, and width is not, so we need to rotate it and THEN
    # flip it. This can be accomplished in one go by transposing:
    image = image.transpose
    width, height = height, width
  else
    raise "Image must have 64 pixels on one axis. Actual dimensions are: #{width}x#{height}"
  end
  File.open(target_bin, 'wb') do |f|
    case format
    when :pbm
      # Output the final image as it will be packed for the MCU:
      f.puts "P1\n#{width} #{height}"
      image.each { |line| f.puts line.join(' ') }
    when :asm
      image.map! do |line|
        f.write '.byte 0b'
        f.puts line.each_slice(8).map(&:join).join(', 0b')
      end
    when :bp1, :bp2, :bp3, :bp4
      # A custom compression format...
      # NOTE: None of these are supported by the firmware, yet.
      d = options[:displace]
      d &&= d.to_i
      if d
        # Displace each line before packing it:
        image.map! do |line|
          if d < 0
            head = line.shift(-d)
            line + head
          else
            tail = line.pop(d)
            tail + line
          end
        end
      end
      # Convert the data into an array of byte-sized integers:
      data = image.flatten.each_slice(8).map{|b| b.join.to_i(2)}
      case format
      when :bp1
        # Type 1 compression: Each line has a leading bit-field to show which bytes in the
        # line are used, and hence have been supplied as bytes...
        # Slice that data back into lines and process each:
        data.each_slice(8) do |line|
          # Compress this line:
          bitfield = ''
          bytes = ''
          line.each do |byte|
            if byte == 0
              bitfield << '0'
            else
              bitfield << '1'
              bytes << byte
            end
          end
          # Write out this compressed line:
          line = ([bitfield].pack('B*') << bytes)
          f.write line
        end
      when :bp2
        # Type 2 compression: 4 bits are used to indicate how many following bytes
        # are used or unused, e.g.:
        #   4   0011  4 bytes unused
        #   4   1000  1 byte USED
        #   8           (data)
        #   4   0110  7 bytes unused
        #   4   1000  1 byte USED
        #   8           (data)
        bits = ''
        raise 'bp2 compression not yet implemented!'
      when :bp3
        # Type 3 compression: Same as type 1, except bitfield indicates which bytes
        # have CHANGED compared with the previous line. Could also use 1 extra bit per
        # line to indicate a RESET if it would be cheaper to do so.
        buffer = [0]*8
        data.each_slice(8) do |line|
          # Find out which bytes in THIS line are different from the previous line:
          comp = [buffer,line].transpose
          # Generate the bitfield by comparing pairs:
          bitfield = comp.map{|p| (p[0]==p[1]) ? 0 : 1}.join
          # Pack the bytes that are different:
          bytes = ''
          comp.each {|p| bytes << p[1] if p[0]!=p[1]}
          # Store this line as the new buffer:
          buffer = line
          # Write the compressed line:
          line = ([bitfield].pack('B*') << bytes)
          f.write line
        end
      when :bp4
        # Type 4 compression: Same as type 3, except an extra bit (per line) is 0
        # if normal BP3 applies, or 1 if the whole line gets reset; if doing so will yield
        # fewer "occupied" bytes, this is the better option. Basically, a blank line
        # will always get this.
        reset_bits = ''
        buffer = [0]*8
        data.each_slice(8) do |line|
          # Find out which bytes in THIS line are different from the previous line:
          normal_line = bp4_compare(buffer, line)
          reset_line = bp4_compare([0]*8, line)
          if reset_line.length < normal_line.length
            reset_bits << '1'
            buffer = [0]*8
            f.write reset_line
          else
            reset_bits << '0'
            buffer = line
            f.write normal_line
          end
        end
        f.write [reset_bits].pack('B*')
      end
    when :raw
      # Bit-pack the whole lot as binary:
      f.write [image.flatten.join].pack('B*')
    else
      raise "Format #{format} is not yet supported!"
    end
    size = f.pos
  end
ensure
  # Delete the temporary file we created:
  File.delete(tempfile)
end
puts "Wrote: #{target_bin.inspect}; #{width}x#{height} (#{(width*height)/8} pixel-bytes; #{size} actual bytes)"

