#!/usr/bin/env ruby

require 'net/http'
require 'net/https'
require 'optparse'
require 'uri'

def verbose_printer(verbosity)
  if verbosity
    pid = Process.pid
    Proc.new do |msg|
      $stderr.puts "[#{pid}]: #{msg}"
    end
  else
    Proc.new do |a|
    end
  end
end

def parse_option(argv)
  parser = OptionParser.new
  opt = Hash.new

  print_help = Proc.new do |msg, *args|
    exits = if args.size > 0
              args.shift
            else
              true
            end
    if msg
      puts msg
      puts
    end
    puts "Usage: #{$0} [OPTIONS] HOSTNAME"
    puts parser.help

    if exits
      exit(1)
    end
  end

  parser.on('-h') do
    print_help.call(nil)
  end

  # TODO: should be rewrite in cURL style option
  opt[:verbose] = nil
  parser.on('-v', '--verbose',
            "\n\t\tverbose") do
    opt[:verbose] = true
  end

  opt[:headers] = Hash.new
  parser.on('-H', '--header HEADER',
            "\n\t\tadditional HTTP header field." +
            "\n\t\tuse like -h \"User-Agent: Ruby\"") do |header|
    unless header =~ /^([-A-Za-z]+):\s*(.+)$/
      print_help.call("Invalid HTTP header format: #{header}")
    end
    opt[:headers][$~[1]] = $~[2]
  end

  opt[:method] = 'GET'
  parser.on('-m', '--method METHOD',
            "\n\t\tHTTP request method"+
            " (GET and POST are supported)") do |method|
    unless method =~ /\A(GET|POST)\Z/
      print_help.call("Not such HTTP method: #{method}")
    end
    opt[:method] = method
  end

  opt[:path] = '/'
  parser.on('-P', '--path PATH',
            "\n\t\tHTTP request path (default: \"/\")") do |path|
    opt[:path] = path
  end

  opt[:encode] = nil
  parser.on('-e', '--encode',
            "\n\t\tpercent encode HTTP request path (default: no)") do
    opt[:encode] = true
  end

  opt[:ssl] = nil
  parser.on('-s', '--ssl', "\n\t\tuse SSL (default: no)") do
    opt[:ssl] = true
  end

  opt[:port] = nil
  parser.on('-p', '--port PORT',
            "\n\t\tdestination port number" +
            " (default: 80 on non-SSL, 443 on SSL)") do |port|
    opt[:port] = port.to_i
    puts "port: #{opt[:port]}"
  end

  opt[:ssl_cert] = nil
  parser.on('-c', '--cert PATH',
            "\n\t\tCA certificate file(*.pem)" +
            " for verifying certificates") do |path|
    unless File.exist?(path)
      print_help.call("No such file: #{path}")
    end
    opt[:ssl_cert] = path
  end

  parser.parse!(argv)

  if argv.size == 0
    print_help.call('HOSTNAME required')
  end
  opt[:host] = argv.first
  if !opt[:port]
    opt[:port] = if opt[:ssl]
                   443
                 else
                   80
                 end
  end
  opt[:path] = URI.encode(opt[:path]) if opt[:encode]

  opt
end

def print_header_string(res)
  print "HTTP/#{res.http_version} #{res.code} #{res.message}"
  res.canonical_each do |name,value|
    print("\r\n#{name}: #{value}")
  end
end

def main(argv)
  Net::HTTP.version_1_2
  opt = parse_option(argv)
  vprint = verbose_printer(opt[:verbose])

  http = Net::HTTP.new(opt[:host], opt[:port])
  if opt[:ssl]
    http.use_ssl = true
    vprint.call("SSL enabled")
    if opt[:ssl_cert]
      http.ca_file = opt[:ssl_cert]
      http.verify_mode = OpenSSL::SSL::VERIFY_PEER
      http.verify_depth = 5
    end
  end
  
  res = nil
  begin
    http.start do |client|
      res = case opt[:method]
            when /GET/
              client.get(opt[:path], opt[:headers])
            when /POST/
              client.post(opt[:path], opt[:headers])
            end
    end
  end
  
  if res
    print_header_string(res)
    print "\r\n\r\n"
    print res.body
    $stdout.flush
  else
    $stderr.puts "error"
  end
end

if __FILE__ == $0
  main(ARGV.dup)
end
