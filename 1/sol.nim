import std/[strutils, algorithm, sequtils]

let inp: seq[string] = readAll(open("input.txt")).splitLines()

proc food(foodSeq: seq[string]): seq[int] =
  var current: int = 0
  var output: seq[int] = @[0]
  for entry in foodSeq:
    if entry == "":
      output.add(current)
      current = 0
    else: current += parseInt(entry)
  return output

let elves: seq[int] = food(inp)

echo max(elves)

echo elves.sorted(order=Descending)[0..2].foldl(a+b, 0)
