#!/usr/bin/env ruby

require 'optparse'
require 'tempfile'
require 'pathname'
require 'fileutils'

def y_or_n_p(msg)
  answer = nil

  while answer == nil
    print "#{msg} [yes or no]: "
    $stdout.flush
    ans = $stdin.readline.strip
    case ans
    when /yes/
      answer = true
      break
    when /no/
      answer = false
      break
    else
      puts "Please type 'yes' or 'no'."
      puts
    end
  end

  answer
end

class FileContext
  def initialize(lines, cur_line_idx,
                 match_col_start, match_col_end, rplstr)
    @lines = lines
    @cur_line_idx = cur_line_idx
    @match_col_start = match_col_start
    @match_col_end = match_col_end
    @rplstr = rplstr
  end

  def inspect
    cur_line = @lines[@cur_line_idx][1].dup
    len = @match_col_end - @match_col_start
    rplstr = "\033[1m\033[4m" + cur_line[@match_col_start, len] + "\033[0m"
    cur_line[@match_col_start, len] = rplstr
    lines = @lines.dup
    lines[@cur_line_idx] = [@lines[@cur_line_idx][0], cur_line]
    
    lineno_width = lines[lines.size - 1][0].to_s.size
    lines.map{|line| sprintf("%#{lineno_width}d: %s", line[0], line[1])}.join.rstrip
  end

  def line
    @lines[@cur_line_idx][1]
  end

  def lineno
    @lines[@cur_line_idx][0]
  end

  def replaced_line
    cur_line = @lines[@cur_line_idx][1].dup
    len = @match_col_end - @match_col_start
    cur_line[@match_col_start, len] = @rplstr
    cur_line
  end
end

class FileReplacer
  def initialize(file, opt)
    @infile = File.open(file, "r")
    @outfile = Tempfile.new("fileupdater")
    @opt = opt

    @lineno = 1
    # current line is the first element of after_buffer
    @before_buffer = []
    @after_buffer = []
    (@opt[:context] + 1).times do
      self.fetch_line(true)
    end
  end
  
  def replace!()
    while @after_buffer.size > 0
      pos = 0
      cur_line_str = @after_buffer.first[1]
      while @opt[:regexp].match(cur_line_str[pos..(cur_line_str.size)])
        context = FileContext.new(@before_buffer + @after_buffer,
                                  @before_buffer.size,
                                  pos + $~.begin(0),
                                  pos + $~.end(0), @opt[:replace])
        rpl = yield(context) || cur_line_str
        if rpl != cur_line_str
          @after_buffer[0][1] = rpl
        end
        pos = $~.begin(0) + 1
      end
      @outfile.print(@after_buffer.first[1])

      self.fetch_line
    end
    
    @infile.close
    @outfile.close
    FileUtils.copy(@outfile.path, @infile.path)
  end

  def fetch_line(beginning_of_file = false)
    eof_reached = false
    line = nil
    begin
      line = @infile.readline
    rescue EOFError => e
      eof_reached = true
    end
    
    if line
      @after_buffer.push([@lineno, line])
      @lineno += 1
    end

    if ! beginning_of_file &&
        (eof_reached || @after_buffer.size > @opt[:context] + 1)
      @before_buffer.push(@after_buffer.shift)
      @before_buffer.shift if @before_buffer.size > @opt[:context]
    end

    @after_buffer.first
  end
end

def update(file, opt)
  base = Pathname.pwd
  path = Pathname.new(file).realpath.to_s
  replacer = FileReplacer.new(file,
                              :regexp => Regexp.compile(Regexp.quote(opt[:prev_version])),
                              :replace => opt[:next_version],
                              :context => opt[:context])
  replacer.replace! do |context|
    puts "==== file: #{Pathname.new(path).relative_path_from(base).to_s} ===="
    puts context.inspect
    puts
    puts "\033[34m\033[1mbefore\033[0m: #{context.line}"
    puts "\033[31m\033[1mafter\033[0m : #{context.replaced_line}"
    puts
    if y_or_n_p("Update this line?")
      context.replaced_line
    else
      context.line
    end
  end
rescue Errno::ENOENT => e
  nil
end

def parse_args(argv)
  parser = OptionParser.new
  opt = Hash.new
  print_help = Proc.new do |msg|
    puts msg
    puts
    puts parser.help
    exit
  end

  opt[:context] = 3
  parser.on('-C', '--context=NUM',
            "\n\t\tPrint NUM lines around matching lines."+
            " Default value is #{opt[:context]}") do |num|
    opt[:context] = num.to_i
    opt[:context] = 3 if opt[:context] < 3
  end

  parser.on('-p', '--prev-version=NUM',
            "\n\t\tPrevious version number.") do |prev|
    opt[:prev_version] = prev
  end

  parser.on('-n', '--next-version=NUM',
            "\n\t\tNext version number.") do |next_|
    opt[:next_version] = next_
  end

  parser.parse!(argv)

  unless opt[:prev_version]
    print_help.call("Previous version number must be specified")
  end

  unless opt[:next_version]
    print_help.call("Next version number must be specified")
  end

  opt
end

def main(argv)
  opt = parse_args(argv)

  argv.each do |file|
    update(file, opt)
  end
end

if __FILE__ == $0
  main(ARGV.dup)
end
