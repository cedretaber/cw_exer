# -*- encoding: utf-8 -*-

@mode2outcom = {
  passed: "Passed",
  ignored: "Ignored",
  failed: "Failed",
  errored: "NotRunnable"
}.tap { |h| h.default = "None" }

def test(mode, line)
  if /^(?<name>.*)\[(?<file_name>.*):(?<line_number>\d+)\]$/ =~ line
    file_name = file_name.empty? ? "" : %Q(-FileName "#{file_name}:#{line_number}")
    outcome = @mode2outcom[mode]
    unless name.empty?
      message = %Q!appveyor AddTest "#{name.strip}" -Framework NUnit #{file_name} -Outcome #{outcome}!
      `#{message}`
    end
  end
end

mode = nil

while line = gets
  line.chomp!.strip!

  case line
  when /^Passed:/
    mode = :passed
    next
  when /^Ignored:/
    mode = :ignored
    next
  when /^Failed:/
    mode = :failed
    next
  when /^Errored:/
    mode = :errored
    next
  when "<Expecto>"
    mode = nil
    next
  end

  test(mode, line) if mode
end