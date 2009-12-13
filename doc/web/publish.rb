#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'rubygems'
require 'bluecloth'

$web_dir = File.dirname(__FILE__)

$html_header = "
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
   \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"ja\" lang=\"ja\">
  <head>
    <title>Twittering-mode</title>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
  </head>
  <body>
"
$html_footer = "
  </body>
</html>
"

textfile = nil
htmlfile = nil

ARGV.each do |arg|
  next unless File.exist?(arg)
  textfile = File.open(arg)
  if arg =~ /\.text$/
    htmlfile = arg.gsub(/\.text$/, ".html")
  else
    htmlfile = arg + ".html"
  end
  htmlfile = File.open(htmlfile, "w")
  break
end

unless textfile
  textfile = $stdin
  htmlfile = $stdout
end

html_body = BlueCloth.new(textfile.read).to_html
htmlfile.puts($html_header + html_body + $html_footer)



