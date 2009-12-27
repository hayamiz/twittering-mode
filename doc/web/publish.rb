#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'optparse'
require 'tempfile'
require 'erb'
require 'rubygems'
require 'bluecloth'

$web_dir = File.dirname(__FILE__)

$default_header = <<EOS
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="ja" lang="ja">
  <head>
    <title>html document</title>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
  </head>
  <body>
EOS

$default_footer = <<EOS
  </body>
</html>
EOS

def header(options)
  b = binding
  eval("include ERB::Util", b)
  options[:data].each do |data|
    eval(data, b)
  end
  erb = if options[:header]
          ERB.new(File.open(options[:header]).read)
        else
          ERB.new($default_header)
        end
  erb.result(b)
end

def footer(options)
  b = binding
  options[:data].each do |data|
    eval(data, b)
  end
  erb = if options[:footer]
          ERB.new(File.open(options[:footer]).read)
        else
          ERB.new($default_footer)
        end
  erb.result(b)
end

def parse_option(argv)
  opt = OptionParser.new
  opt.banner = "Usage: #{$0} [options] MARKDOWN-FILE"

  options = Hash.new
  options[:po_file] = nil
  opt.on('-p', '--po-file=FILE',
         "\n\t\tTranslate with FILE by gettext") do |v|
    
    unless File.exist?(v)
      $stderr.puts("No such po file: #{v}")
      $stderr.puts()
      puts opt.help()
      exit(1)
    end
    options[:po_file] = v

    unless system("which xml2po 1> /dev/null 2>/dev/null")
      $stderr.puts("'xml2po' command is required.")
      $stderr.puts()
      $stderr.puts("If you are using Debian or Ubuntu,\n'xml2po' can be installed by the following command:\n    apt-get install gnome-doc-utils")
      exit(1)
    end
  end

  options[:output] = nil
  opt.on('-o FILE', '--output=FILE',
         "\n\t\tPrint resulting text into FILE") do |file|
    options[:output] = file
  end
  
  options[:header] = nil
  opt.on('-H', '--header=FILE',
         "\n\t\tHeader ERB template. Define parameters by --data option") do |file|
    unless File.exist?(file)
      $stderr.puts("No such file: #{file}")
      $stderr.puts()
      puts opt.help()
      exit(1)
    end
    options[:header] = file
  end

  options[:footer] = nil
  opt.on('-F', '--footer=FILE',
         "\n\t\tFooter ERB template. Define parameters by --data option") do |file|
    unless File.exist?(file)
      $stderr.puts("No such file: #{file}")
      $stderr.puts()
      puts opt.help()
      exit(1)
    end
    options[:footer] = file
  end

  options[:data] = Array.new
  opt.on('-d', '--data=NAME=VALUE',
         "\n\t\tBind VALUE(any ruby expression) to NAME when rendering HTML.") do |data|
    if data =~ /([^=])=(.+)/
      options[:data] << data
    end
  end

  opt.parse!(argv)

  options
end

def main(argv)
  textfile = nil
  output = nil
  
  options = parse_option(argv)

  argv.each do |arg|
    next unless File.exist?(arg)
    textfile = File.open(arg)

    if options[:output]
      output = File.open(options[:output], "w")
    else
      output = $stdout
    end
    break
  end
  
  unless textfile
    textfile = $stdin
    output = $stdout
  end
  
  tempfile = Tempfile.new("twmode-publish")

  html_body = BlueCloth.new(textfile.read).to_html
  tempfile.puts(header(options) + html_body + footer(options))
  tempfile.fsync()

  translated_str = nil
  if options[:po_file]
    translated_str = `xml2po -k -m xhtml -p #{options[:po_file]} -o /dev/stdout #{tempfile.path}`
  else
    translated_str = File.open(tempfile.path).read
  end
  
  translated_str = translated_str.gsub(/\A\s*<\?xml\s*version="1\.0"\s*encoding="utf-8"\s*\?>/, '')

  output.puts(translated_str)
  
  true
end

if __FILE__ == $0
  exit(main(ARGV))
end
