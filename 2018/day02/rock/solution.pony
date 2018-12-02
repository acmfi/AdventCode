use "files"
use "collections"
use "itertools"

actor Main
  new create(env: Env) =>
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
    try
      let fpath = FilePath(env.root as AmbientAuth, "./input", caps)?
      with file = OpenFile(fpath) as File
      do
        let words = Array[String]
        for l in file.lines() do
          words.push(consume l)
        end

        (let w1, let w2) = find_almost_equals(words)?

        let equals = all_equals(w1, w2)
        env.out.print(equals)
      end
    else
      env.out.print("Can't open it")
    end

  fun all_equals(s1: String, s2: String): String =>
    let result: Array[U8] val = recover val
      Iter[U8](s1.array().values())
      .zip[U8](s2.array().values())
      .filter({(x) =>
        (let x1, let x2) = x
        x1 == x2
      })
      .map[U8]({(x) =>
        (let x1, _) = x
        x1
      })
      .collect(Array[U8]())
    end

    String.from_array(result)

  fun find_almost_equals(words: Array[String]): (String, String)? =>
    var n: USize = words.size()

    var c1: USize = 0


    while c1 < n do
      var c2: USize = c1 + 1
      while c2 < n do
        let w1 = words(c1)?
        let w2 = words(c2)?

        if almost_equal(w1, w2) then
          return (w1, w2)
        end
        c2 = c2 + 1
      end

      c1 = c1 + 1
    end
    error


  fun almost_equal(s: String, s2: String): Bool =>
    var diff: U8 = 0
    for k in s.array().keys() do
      try
        if s(k)? != s2(k)? then
          diff = diff + 1
          if diff >= 2 then
            return false
          end
        end
      else
        return false
      end
    end
    true
