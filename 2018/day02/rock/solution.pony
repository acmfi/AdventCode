use "files"
use "collections"
use "format"
// use "inspect"

primitive NoneV
primitive TwoV
primitive ThreeV
primitive BothV

type SumV is (BothV | ThreeV | TwoV | NoneV)

actor Main
  new create(env: Env) =>
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
    try
      let fpath = FilePath(env.root as AmbientAuth, "../input", caps)?
      with file = OpenFile(fpath) as File
      do
        var two: U64 = 0
        var three: U64 = 0
        for line in file.lines() do
          let a = analyze(consume line)
          // env.out.print(Inspect(a))
          let sum = sum_values(a)
          (two, three) = match sum
            | TwoV => (two + 1, three)
            | ThreeV => (two, three + 1)
            | BothV => (two + 1, three + 1)
            else
              (two, three)
            end
          end

          // env.out.print(Inspect(two))
          // env.out.print(Inspect(three))
          env.out.print(Format.int[U64](two * three))
      end
    else
      env.out.print("Can't open it")
    end

  fun analyze(s: String): Map[U8, U64] =>
    var data = Map[U8, U64]()
    for c in s.array().values() do
      try
        data.upsert(c, 1, {(x, y) => x + y})?
      else
        data
      end
    end
    data

  fun sum_values(h: Map[U8, U64]): SumV =>
    var s: SumV = NoneV
    for v in h.values() do
      s = match (v, s)
        | (2, NoneV) => TwoV
        | (2, ThreeV) => BothV
        | (2, _) => s
        | (3, NoneV) => ThreeV
        | (3, TwoV) => BothV
        | (3, _) => s
        else
          s
        end
    end

    s
