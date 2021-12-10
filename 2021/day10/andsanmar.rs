fn points(c : char) -> usize {
    match c {        
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => panic!()
    }
}

fn points_uncomplete(c : char) -> usize {
    match c {        
        ')' => 1,
        ']' => 2,
        '}' => 3,
        '>' => 4,
        _ => panic!()
    }
}

fn contr(c : char) -> char {
    match c {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        _ => panic!()
    }
}


fn value_line(l : &Vec<char>, pos : usize) -> Result<(usize,usize),usize> { // Format: (position,accumulated punctuation),Malformed
    if pos >= l.len() { return Ok((pos,0)) }
    match l[pos] {
        '{' | '(' | '<' | '[' => match value_line(l, pos+1) {
            Ok((p,acc_p)) => {
                if p >= l.len() { Ok((p,acc_p*5+points_uncomplete(contr(l[pos])))) }
                else if l[p] != contr(l[pos]) { Err(points(l[p])) }
                else { value_line(l,p+1) }
            },
            Err(y) => Err(y)
        },
        _ => Ok((pos,0)),
    }
}

fn stars(s : &Vec<String>) {
    let values : Vec<Result<(usize,usize),usize>> = s.iter().map(|l| value_line(&l.chars().collect(), 0)).collect();
    let star1 = values.iter().filter_map(|n| match n {
        Err(i) => Some(i),
        _ => None
    });
    println!("{:?}", star1.sum::<usize>());
    let mut star2 : Vec<usize> = values.iter().filter_map(|n| match n {
        Ok((_a,b)) => Some(*b),
        _ => None
    }).collect();
    star2.sort();
    println!("{:?}", star2[star2.len()/2]);
}


fn parse_input(s : String) -> Vec<String> {
    s.lines().filter_map(|l| {
        let r = l.to_string();
        match r.len() {
            0 => None,
            _ => Some(r)
        }
    }).collect()
}


fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let s: Vec<String> = parse_input(std::fs::read_to_string(args[1].clone()).unwrap());
    stars(&s);
}
