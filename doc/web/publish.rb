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
    <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" media=\"screen\" />
  </head>
  <body>
<div id=\"wrap\">

<!--
<div id=\"header\">
<h1><a href=\"#\">Twittering-mode</a></h1>
<h2>A major mode for Twitter.</h2>
</div>
<div id=\"top\"> </div>
-->

<div id=\"menu\">
<ul>
<li><img src=\"./images/logo.png\" /></li>
</ul>
</div>

<div id=\"content\">
<div class=\"left\">
"
$html_footer = "
</div>
<div style=\"clear: both;\"> </div>
</div>

<div id=\"bottom\"> </div>
<div id=\"footer\">
Designed by <a href=\"http://www.free-css-templates.com/\">Free CSS Templates</a>, Thanks to <a href=\"http://www.legalhelpers.com/chapter-13-bankruptcy/chapter13.html\">Chapter 13 Bankruptcy</a>
</div>
</div>

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



