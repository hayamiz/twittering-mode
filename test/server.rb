#!/usr/bin/env ruby

require 'optparse'
require 'webrick'
require 'webrick/httpproxy'
require 'singleton'

$alpha = ('A'..'Z').map{|c|c[0]} + ('a'..'z').map{|c|c[0]}
$num = ('0'..'9').map{|c|c[0]}
$alphanum = $alpha + $num
$screen_name = $alphanum + '_'[0]
$ascii = (32..126).to_a

$hiragana = (0x3040..0x309F).to_a
$katakana = (0x30A0..0x30FF).to_a
$cjk_kanji = (0x4E00..0x9FFF).to_a

def random_string(chars, length_min, length_max)
  len = length_min + rand(length_max - length_min + 1)
  Array.new(len).map{ |x|
    chars.choice
  }.pack('U*')
end

class User
  def self.generate(id = nil)
    @users = Hash.new unless @users

    id = rand(1000000) unless id
    user = @users[id]
    unless user
      user = self.new(id)
    end
  end

  attr_accessor :attrs
  def initialize(id)
    @attrs = Hash.new
    @attrs[:id] = id
    @attrs[:name] = 
  end
end

class TwitterAPI
  include Singleton

  attr_accessor :res
  attr_accessor :req

  def self.call(req, res)
    uri = req.request_uri
    format = File.extname(uri.path)
    format = "xml" if format == ""
    format = format.gsub(/^\./, '')

    endpoint = uri.path.gsub(/\.[a-z]+$/, '').gsub(/^\//, '')

    params = Hash.new
    (uri.query || "").split("&").each do |param|
      if param =~ /([^=]+)=(.+)/
        params[$~[1]] = $~[2]
        params[$~[1].to_sym] = $~[2]
      else
        raise ArgumentError.new("Invalid query string: #{param}")
      end
    end
    
    tw = self.instance
    tw.res = res
    tw.req = req
    tw.send(endpoint.gsub(/\//, '_'), format, params)
  end

  def statuses_public_timeline(format, params)
    @res.body = "URI: #{@req.request_uri.to_s}, format: #{format}, params: #{params.inspect}"
  end
end

def parse_options(argv)
  parser = OptionParser.new
  opt = Hash.new
  
  print_help = Proc.new do |msg|
    $stderr.puts msg
    puts
    puts parser.help
    exit 1
  end

  opt[:port] = 80
  parser.on('-p', '--port=PORT',
            "\n\t\tPort number to be used.") do |port|
    opt[:port] = port.to_i
    if opt[:port] == 0
      print_help.call("Invalid port number: #{port}")
    end
  end
  
  opt[:proxy] = false
  parser.on('-P', '--proxy',
            "\n\t\tRun as a proxy server if this option is specified.") do
    opt[:proxy] = true
  end

  parser.parse!(argv)
  
  opt
end

def main(argv)
  opt = parse_options(argv)

  server = WEBrick::HTTPServer.new({:Port => opt[:port],
                                     :BindAddress => "127.0.0.1"})
  server.mount_proc("/") do |req, res|
    TwitterAPI.call(req, res)
  end

  Signal.trap('INT') do
    server.shutdown
  end
  
  server.start
end

if __FILE__ == $0
  main(ARGV.dup)
end
