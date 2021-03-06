# -*- encoding: UTF-8 -*-

def error(*msg)
  print "ERROR: ", msg.join, $/
  exit -1
end

if ARGV.size == 0 then
  error "config not found", $/, "Usage: ndbot.rb config"
end

config = ARGV.shift

require 'time'

require 'rubygems'
require 'oauth'

PATH = File.dirname(__FILE__)
CACHE_FILE = PATH + "/ndcache.txt"

KEYS = ["consumer_key", "consumer_secret", "access_token", "access_token_secret"]

token = {}
IO.readlines(config).each {|l|
  ls = l.chomp.split(/\s*:\s*/,2)
  next if ls.size != 2
  next unless KEYS.include? ls[0]
  token[ls[0]] = ls[1]
}

if !KEYS.reduce {|r, k| r = r && token.include?(k) } then
  error "Do not have enough keys for OAuth. (#{KEYS * ", "})"
end

def parseline(l)
  t = l.chomp.split ","
  {:title => t[0], :link => t[1], :timestamp => Time.parse(t[2])}
end
newwords = `#{PATH}/nicodicbot #{config}`.lines.map {|l| parseline(l) }

# 前回取得した単語リストを読み込む
prevwords = IO.readlines(CACHE_FILE).map {|l| parseline(l) }

# 新着単語の中から，前回チェックした単語リストにある単語を除外
src = newwords.select {|w|
  prevwords.none? {|p| w[:title] == p[:title] }
}

# 今回取得した単語リストを書き出す
open(CACHE_FILE, "w") { |file|
  newwords.each do |i|
    file.write([i[:title], i[:link], i[:timestamp]] * ',' + $/)
  end
}

# Twitterに投稿
consumer = OAuth::Consumer.new(
     token["consumer_key"],
     token["consumer_secret"],
     :site => 'https://api.twitter.com'
)

access_token = OAuth::AccessToken.new(
     consumer,
     token["access_token"],
     token["access_token_secret"]
)

src.each do |item|
     message = ""
     message << '『'
     message << item[:title]
     message << '』 '
     message << item[:link]
     response = access_token.post(
          'https://api.twitter.com/1.1/statuses/update.json',
          'status' => message
     )
end
