#!/usr/bin/env ruby

# takes 2 arguments: first is the path to the dat file from which to build the Haskell vectors
# second argument is the name to assign it to.

table = {}
alphabet = "ACDEFGHIKLMNPQRSTVWYX"
File.foreach(ARGV[0]) do |line|
  letters, score = line.chomp.split(/\s+/)
  first, second = letters.split('').map{|l| alphabet.index(l)}
  # score is a log, we want negative logs
  score = -1 * score.to_f
  table[first] ||= {}
  table[first][second] = score
end
name = ARGV[1]
padding = name.length + 12

puts "#{name} = fromList [\n" +
(0...(alphabet.length)).map { |i|
  "#{' '*padding}fromList [ " + 
  ((0...(alphabet.length)).map {|j| table[i][j] }).join(', ') +
  " ]"
  }.join(",\n") +
"\n#{' '*padding}]"
