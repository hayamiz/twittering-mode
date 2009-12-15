#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'optparse'
require 'tempfile'
require 'rubygems'
require 'bluecloth'

$web_dir = File.dirname(__FILE__)

def header(options)
  lang = options[:lang]
  
  langs = $available_langs.dup
  # langs.delete(lang)
  
  page = File.basename(options[:output] || "index.html")
    
  lang_menu_items = langs.map do |l|
    rel_dir = "../#{l}"
    if lang == "en" && l == "en"
      rel_dir = "."
    elsif lang == "en"
      rel_dir = "./#{l}"
    elsif l == "en"
      rel_dir = ".."
    end

    "<a href=\"#{rel_dir}/#{page}\"><img src=\"#{rel_dir}/images/lang-#{l}.png\" /></a>"
  end.join("&nbsp;")
  
  ret = <<EOS
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="#{lang}" lang="#{lang}">
  <head>
    <title>Twittering-mode</title>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <link rel="stylesheet" type="text/css" href="style.css" media="screen" />
  </head>
  <body>
<div id="wrap">

<!--
<div id="header">
<h1><a href="#">Twittering-mode</a></h1>
<h2>A major mode for Twitter.</h2>
</div>
<div id="top"> </div>
-->
<div style="text-align: left; padding-left: 20px;">
#{lang_menu_items}
</div>

<div id="top"> </div>

<div id="menu">
<ul>
<li><a href="./index.html"><img src="./images/logo.png" /></a></li>
</ul>
</div>

<div id="content">
<div class="left">
EOS

  ret
end

def footer(options)
  <<EOS
</div>
<div style="clear: both;"> </div>
</div>

<div id="bottom"> </div>
<div id="footer">
Designed by <a href="http://www.free-css-templates.com/">Free CSS Templates</a>, Thanks to <a href="http://www.legalhelpers.com/chapter-13-bankruptcy/chapter13.html">Chapter 13 Bankruptcy</a>
<br />
<a href="http://www.famfamfam.com/"><img src="images/famfamfam-logo.png" /></a>
</div>
</div>

  </body>
</html>
EOS
end

$available_langs = ["en"]
Dir.glob("#{$web_dir}/??.po").map{|f| File.basename(f)}.each do |catalog|
  lang = catalog.gsub(/\.po$/, '')
  $available_langs << lang
end
$available_langs.sort!.uniq!

unless system("which xml2po 1> /dev/null 2>/dev/null")
  $stderr.puts("'xml2po' command is required.")
  $stderr.puts()
  $stderr.puts("If you are using Debian or Ubuntu,\n'xml2po' can be installed by the following command:\n    apt-get install gnome-doc-utils")
  exit(1)
end

def parse_option(argv)
  opt = OptionParser.new
  
  options = Hash.new
  options[:lang] = "en"
  opt.on('-l LANG', '--lang=LANG',
         "Language to be translated to (#{$available_langs.join(", ")})") do |v|
    unless $available_langs.include?(v)
      $stderr.puts("Unknown language: #{v}")
      $stderr.puts()
      puts opt.help()
      exit(1)
    end
    options[:lang] = v
  end

  options[:output] = nil
  opt.on('-o FILE', '--output FILE',
         'Print resulting text into FILE') do |file|
    options[:output] = file
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

    unless options[:output]
      if arg =~ /\.text$/
        options[:output] = arg.gsub(/\.text$/, ".html")
      else
        options[:output] = arg + ".html"
      end
    end
    output = File.open(options[:output], "w")
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
  if options[:lang] != "en"
    translated_str = `xml2po -k -m xhtml -p #{options[:lang]}.po -o /dev/stdout #{tempfile.path}`
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
