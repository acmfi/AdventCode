#[derive(Debug)]
enum FS {
    Dir(String, Vec<FS>),
    File(String, usize),
}

use crate::FS::*;

impl FS {
    fn weight(&self) -> u64 {
        match self {
            Dir(_, v) => v.iter().map(|e| e.weight()).sum(),
            File(_, w) => *w as u64
        }
    }

    fn find_min(&self, min : u64, current : u64) -> u64 {
        let own = self.weight();
        if let Dir(_, v) = self {
            if own > min {
                let a = v.iter().map(|e| e.find_min(min, own)).min().unwrap();
                if a >= own {
                    own
                } else {
                    a
                }
            } else {
                current
            }
        } else {
            current
        }
    }
}

type Struct = FS;

fn star1(l : &Struct) -> u64 {
    let w = l.weight();
    let mut final_weight : u64 = 0;
    match l {
        Dir(_,v) => {
            final_weight += v.iter().map(|e| star1(e)).sum::<u64>();
            if w < 100000 {
                final_weight += w;
            }
        },
        _ => {}
    }
    final_weight
}

fn star2(l : &Struct) {
    let needed = 30000000 - (70000000 - l.weight());
    let min = l.find_min(needed, l.weight());
    println!("{:?}", min);
}

fn parse(input: &Vec<&str>, mut index: usize) -> (Option<FS>, usize) {
    if input[index] == "$ cd .."  {
        panic!("malformed input");
    }
    if input[index].is_empty() {
        return (None, index+1)
    }
    let tokens : Vec<&str> = input[index].split_whitespace().collect();
    match tokens[0] {
        "$" => {
            if tokens[1] == "cd" && tokens[2] != ".." {
                index += 1;
                let mut vec : Vec<FS> = vec![];
                while index < input.len() && input[index] != "$ cd .." {
                    match parse(input, index) {
                        (Some(fs), ni) => {
                            vec.push(fs);
                            index = ni;
                        },
                        (None, ni) => {
                            index = ni;
                        }
                    }
                }
                (Some(Dir(tokens[2].to_string(), vec)), index+1)
            } else {
                (None, index + 1)
            }
        },
        "dir" => {
            (None, index + 1)
        },    
        _ => (Some(File(tokens[1].to_string(), tokens[0].parse().unwrap())), index + 1)
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Struct = parse(&std::fs::read_to_string(&args[1].clone()).unwrap().lines().collect(), 0).0.unwrap();

    println!("{:?}", star1(&l));
    star2(&l);
}
