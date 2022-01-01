use std::collections::HashMap;

#[derive(Debug,Copy,Clone)]
enum Reg {
    X, Y, Z, W, Lit(i64)
}

#[derive(Debug,Copy,Clone)]
enum Inst {
    Inp(Reg),
    Add(Reg,Reg),
    Mul(Reg,Reg),
    Mod(Reg,Reg),
    Div(Reg,Reg),
    Eql(Reg,Reg)
}

type Monad = Vec<Inst>;

fn get_index(reg : &Reg) -> usize {
    match reg {
        Reg::X => 0,
        Reg::Y => 1,
        Reg::Z => 2,
        Reg::W => 3,
        _ => panic!()
    }
}

fn get_value(reg : &Reg, regs : &[i64;4]) -> i64 {
    match reg {
        Reg::Lit(n) => *n,
        _ => regs[get_index(reg)]
    }
}


fn emulate_all(monad : &Monad) -> Vec<([i64;4],(usize,usize))> {
    let mut states : Vec<([i64;4],(usize,usize))> = vec![([0;4],(0,0))];
    for inst in monad {
        match inst {
            Inst::Inp(r) => {
                let mut dedup : HashMap<[i64;4],(usize,usize)> = HashMap::new();
                for (mut regs, (min,max)) in states {
                    regs[get_index(r)] = 0;
                    let entry = dedup.entry(regs).or_insert((min,max));
                    entry.0 = entry.0.min(min);
                    entry.1 = entry.1.max(max);
                }
                states = dedup.iter().map(|(a,b)| (*a,*b)).collect();
                let r_i = get_index(r);
                let mut next : Vec<([i64;4],(usize,usize))> = Vec::with_capacity(states.len() * 9);
                for (regs, (min, max)) in states.iter() {
                    for value in 1..=9 {
                        let mut regs = *regs;
                        regs[r_i] = value;
                        let min = min * 10 + value as usize;
                        let max = max * 10 + value as usize;
                        next.push((regs, (min, max)));
                    }
                }
                states = next;
            }
            Inst::Add(r, v) => {
                for (regs, _) in states.iter_mut() {
                    let val : i64 = get_value(v,&regs);
                    let r_i = get_index(r);
                    regs[r_i] += val;
                }
            },
            Inst::Mul(r, v) => {
                for (regs, _) in states.iter_mut() {
                    let val : i64 = get_value(v,&regs);
                    let r_i = get_index(r);
                    regs[r_i] *= val;
                }
            },
            Inst::Mod(r, v) => {
                for (regs, _) in states.iter_mut() {
                    let val : i64 = get_value(v,&regs);
                    let r_i = get_index(r);
                    regs[r_i] %= val;
                }
            },
            Inst::Div(r, v) => {
                for (regs, _) in states.iter_mut() {
                    let val : i64 = get_value(v,&regs);
                    let r_i = get_index(r);
                    regs[r_i] /= val;
                }
            },
            Inst::Eql(r, v) => {
                for (regs, _) in states.iter_mut() {
                    let val : i64 = get_value(v,&regs);
                    let r_i = get_index(r);
                    regs[r_i] = if regs[r_i] == val {1} else {0};
                }
            }
        }
    }
    states
}

fn stars(monad : &Monad) {
    let all_results : Vec<([i64;4],(usize,usize))> = emulate_all(monad);
    let r : Vec<(usize,usize)> = all_results.iter().filter(|([_,_,z,_],_)| *z == 0).map(|(_,bounds)| *bounds).collect();
    
    println!("{:?}", r.iter().map(|(_,max)| max).max().unwrap());
    println!("{:?}", r.iter().map(|(min,_)| min).min().unwrap());
}

fn parse_input<'a>(s : &'a String) -> Monad {
    s.lines().filter_map(|l| {
        if l.len() == 0 {return None}
        let mut i = l.split_whitespace();
        let mnemo = i.next();
        let regs : Vec<Reg> = i.map(|r| { match r {
            "x" => Reg::X,
            "y" => Reg::Y,
            "z" => Reg::Z,
            "w" => Reg::W,
            _ => match r.parse() {
                Ok(n) => Reg::Lit(n),
                _ => panic!()
            }
        }}).collect();
        match mnemo {
            Some("inp") => Some(Inst::Inp(regs[0])),
            Some("add") => Some(Inst::Add(regs[0],regs[1])),
            Some("mul") => Some(Inst::Mul(regs[0],regs[1])),
            Some("mod") => Some(Inst::Mod(regs[0],regs[1])),
            Some("div") => Some(Inst::Div(regs[0],regs[1])),
            Some("eql") => Some(Inst::Eql(regs[0],regs[1])),
            _ => None
        }
    }).collect()
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let filename = std::fs::read_to_string(args[1].clone()).unwrap();
    let s: Monad = parse_input(&filename);
    stars(&s);
    // stars();
}
