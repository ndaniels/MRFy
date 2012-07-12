#!/usr/bin/env ruby
require 'rubygems'
require 'statsample'
# collect time, score, & num generations run

# Run each test 10 times; collect mean, median, min, max, stddev score. Show age of gen for min and median.
numRuns = 10
# For each fasta/hmm+ combination, sort results by min score, descending.

@input_prefix = ARGV[0] || "testing"

@output_prefix = ARGV[1] || "out"

@mrfy_path = ARGV[2] || "mrfy"

@base_name = ARGV[3] || "8"

constArgs = " +RTS -N12"

io_pairs = {
    "8" => ["8.hmm+", "8.fasta"], # positive
    # ["7.hmm+", "7.fasta"], # positive
    # ["6.hmm+", "6.fasta"], # positive
    "barwin" => ["barwin.hmm+", "barwin.fasta"], # positive
    "sandwich" => ["sandwich.hmm+", "sandwich.fasta"] # positive
    # ["8.hmm+", "double_thermo.fasta"], # negative
    # ["51182_smurf-lite.hmm+", "thermotoga_Q9X0A3.fasta"], # negative
    # ["7.hmm+", "8.fasta"], # negative
    # ["7.hmm+", "6.fasta"], # negative
    # ["6.hmm+", "7.fasta"], # negative
    # ["barwin.hmm+", "8.fasta"], # negative
}

convergences = [100, 200, 500, 1_000]
# convergences = [20]

ga_parameters = {
    :multipop => [1]
    :popsize => [500, 1000, 10_000],
    :gens => [10, 20, 50, 100, 500],
    :converge => [5, 10, 20, 50, 100]
}

sa_parameters = {
    # :multipop => [1, 2, 5, 10, 100],
    :multipop => [10],
    :gens => [10_000],
    :inittemp => [10, 100, 1_000, 10_000, 100_000],
    :coolfact => [0.75, 0.9, 0.99, 0.999, 0.99999],
    # :boltz => [1.0e-40, 1.38e-23, 1.0e-10, 1.0e-5, 1.0e-2, 1.0e-1],
    :boltz => [1e-20, 1.0],
    :converge => convergences
}

rhc_parameters = {
    :multipop => [10],
    :gens => [10_000],
    :converge => convergences
}

# debug
# rhc_parameters = {
#     :multipop => [1, 5],
#     :gens => [500],
#     :converge => convergences
# }


search_algorithms = {
    :simanneal => sa_parameters,
    :genetic => ga_parameters,
    :random => rhc_parameters
}

def argify(flag, val)
    "--#{flag.to_s} #{val}"
end

def arglist(flag, values)
    values.map{|v| argify(flag, v)}
end

def mkOutFileName(args)
    x=args.inject('') do |s,a|
        s += a.gsub(/\-\-(\w)\w*(\s+([\w\.-]+))?/, '\1\2').
        gsub(@input_prefix, '').
        gsub(@output_prefix, '').
        gsub('/', '').gsub(/\s+/,'')
    end
    x += ".out"
    x
end

counter = 0
allRunArgs = []
STDERR.puts "Generating runs..."
search_algorithms.each_pair do |alg, params|
    # we want the cartesian product
    # allParams will be a list of lists of argument terms
    allParams = params.to_a.map{|p| arglist(p[0], p[1])}
    # we'll want its cartesian product
    # allArgs is a list of lists of individual-run arguments
    allArgs = allParams[0].product(*allParams[1..-1])
    allArgs.each do |argsForRun|
        # now, multiply by files needed
        # io_pairs.each do |iop|
        iop = io_pairs[@base_name]
        fileSpecificArgs = argsForRun.dup
        # add the algorithm argument
        fileSpecificArgs << argify(alg, "")
        runDescriptor = fileSpecificArgs.dup + iop
        inFiles = iop.map{|f| File.join(@input_prefix, f)}
        fileSpecificArgs += inFiles
        outFileName = mkOutFileName(fileSpecificArgs)
        outFile = File.join(@output_prefix, outFileName)
        # fileSpecificArgs << outFile
        allRunArgs << [fileSpecificArgs.join(" "), runDescriptor, outFile]
        counter += 1
    end
end
# puts allRunArgs.inspect
STDERR.puts "#{counter} runs generated"

def analyze(file)
    # should return score and age
    score = 1.0e100 # infinity
    File.foreach(file) do |line|
        if line =~ /^Score: (\d*\.\d*)/
            score = Regexp.last_match[1].to_f
        end
    end
    score
end

def getStats(resultSet)
    scores = resultSet.map{|v| v[:score]}.to_scale
    times = resultSet.map{|v| v[:time]}.to_scale

    { :score => { :min => scores.min,
                  :max => scores.max,
                  :stddev => scores.sd,
                  :median => scores.median,
                  :mean => scores.mean
                },
     :time => { :min => times.min,
                :max => times.max,
                :stddev => times.sd,
                :median => times.median,
                :mean => times.mean
              }
    }
end

header =  ["min score", "max score", "std score", "median score", "mean score",
           "min time", "max time", "std time", "median time", "mean time"
          ]

def stringifyResult(result)
    r = []
    [:score, :time].each do |h1|
        [:min, :max, :stddev, :median, :mean].each do |h2|
            r << result[:stats][h1][h2]
        end
    end
    r << result[:desc]
    r.join(',')
end

allResults = []
completed = 0
allRunArgs.each do |argList|
    args, descriptor, outfile = *argList
    results = []
    numRuns.times do |run|
        realOutFile = outfile + run.to_s
        command = "#{@mrfy_path} #{args} #{realOutFile} #{constArgs}"
        # puts "executing #{command}"
        t0 = Time.now
        system(command)
        elapsed = Time.now - t0 # in seconds
        score = analyze(realOutFile)
        results << {:score => score, :time => elapsed}
    end
    completed += 1

    if completed % 100 == 0
       STDERR.puts "#{completed} of #{allRunArgs.length} done"
    end
    # now compute the statistics for results
    # mean, median, min, max, stddev score. Show age of gen for min and median.
    runResults = getStats(results)
    allResults << {:stats => runResults, :desc => descriptor}
end
puts header.join(',')
allResults.sort_by{ |r| [r[:desc][0], r[:desc][1], r[:stats][:score][:median]] }.each do |result|
    puts stringifyResult(result)
end
# sort them by the last two entries in the runDescriptor, then score
# and output them with informative summaries