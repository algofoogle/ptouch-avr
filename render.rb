#!/usr/bin/env ruby
require 'csv'
require 'optparse'

# Process command-line options:
options = {}
OptionParser.new do |o|
  o.banner = "Usage: ruby render.rb [options] [INFILE]\nNOTE: If INFILE is not specified, reads from STDIN."
  o.on('-d', '--displace N', Integer, 'Fix alignment by displacing by N dots (0..64)') do |v|
    options[:displace] = v
  end
  o.on('-o', '--out-file OUTFILE', 'Write output to OUTFILE instead of STDOUT') do |v|
    options[:outfile] = v
  end
  o.on('-H', '--[no-]header', 'Source CSV has a header line. Default: yes') do |v|
    options[:header] = v
  end
  o.on('-f', '--format FORMAT', [:ascii, :pbm],
    'Output format.',
    'One of: ascii (ASCII art); or pbm (Netpbm image).',
    'Default: ascii') do |v|
    options[:format] = v
  end
  o.on('-w', '--line-width WIDTH', Integer, 'Line width. Default: 65') do |v|
    options[:width] = v
  end
  o.on('-s', '--stretch FACTOR', Integer, 'Horizontal stretch FACTOR. Default: 1') do |v|
    options[:stretch] = v
  end
  o.on('-c', '--line-count LINES', Integer, 'Number of lines (for pbm format only). Default: 64') do |v|
    options[:lines] = v
  end
end.parse!

# Handle command-line ARGUMENT (if any):
raise "Too many input file arguments: #{ARGV.inspect}" if ARGV.count > 1
# Determine source file (or stream):
state_file = ARGV.first ? File.open(ARGV.first, 'rb') : STDIN

# Determine output file:
out_file = options[:outfile] ? File.open(options[:outfile], 'wb') : STDOUT

offset = options[:displace] || 0

# Each printable line is 64 "pixels", plus 1 initialisation bit:
line_width = options[:width] || 65

# Horizontal stretch factor. By default, don't stretch:
stretch = options[:stretch] || 1

format = options[:format] || :ascii

# Set up state tracking variables:
skip = options[:header]
sample_counter = 0
last_value = 0
# Glyphs: a 0 becomes a "space" (-) and a 1 becomes a "mark" (#):
format_data = {
  # Element no. 2 is the 'filler' character for broken lines:
  :ascii => ['-', '#', ' '],
  :pbm => ['0 ', '1 ', '0 ']
}
glyphs = format_data[format]

if format == :pbm
  # Output PBM header:
  out_file.puts 'P1'
  out_file.puts "#{line_width * stretch} #{options[:lines] || 64}"
end

# Pre-render the offset (which is assumed to be partial line time
# BEFORE the capture started):
buffer = ''
offset.times { |n| buffer << (glyphs[2] * stretch) }
# Process each line of the CSV file as a "state":
CSV::Reader.parse(state_file) do |state|
  # Check if this is the first line of the file:
  if skip
    # Skip the header line.
    skip = false
    next
  end
  # Get the sample index for this state (first CSV column):
  sample = state[0].to_i
  # Get the value of this sample (2nd-to-last CSV column):
  value = state[-2].to_i
  # Now render the gap, up to this sample:
  while sample_counter <= sample
    if sample_counter == sample
      # We've now reached our sample, so render its glyph:
      buffer << (glyphs[value] * stretch)
    else
      # We haven't yet reached our sample, so render the prior glyph;
      buffer << (glyphs[last_value] * stretch)
    end
    # End this line and advance to the next, if we hit our line width:
    if ((sample_counter+offset+1)%line_width) == 0
      out_file.puts(buffer)
      buffer = ''
    end
    # Step the sample counter:
    sample_counter += 1
  end
  last_value = value
end
# Final newline:
out_file.puts(buffer)
out_file.close
