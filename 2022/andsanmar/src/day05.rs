type Stacks = Vec<Vec<char>>;
type Moves = Vec<(u64,usize,usize)>;

fn star1(mut s : Stacks, m : &Moves) {
    for (a,b,c) in m {
        for _ in 0..*a {
            let elem = s[*b].pop().unwrap();
            s[*c].push(elem);
        }
    }
    println!("{}", s.iter().map(|l| l.last().unwrap()).collect::<String>());
}

fn star2(mut s : Stacks, m : &Moves) {
    for (a,b,c) in m {
        let mut tmp_stack : Vec<char> = vec![]; // for reversing, ez pz
        for _ in 0..*a {
            let elem = s[*b].pop().unwrap();
            tmp_stack.push(elem);
        }
        for e in tmp_stack.iter().rev() {
            s[*c].push(*e)
        }
    }
    println!("{}", s.iter().map(|l| l.last().unwrap()).collect::<String>());
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let binding = std::fs::read_to_string(args[1].clone()).unwrap();
    let mut input = binding.split("\n\n");
    let a : Stacks = input.next().unwrap().lines().filter_map(|l| {
        if !l.is_empty() {
            Some(l.chars().collect())
        }
        else { None }
    }).collect();

    let moves : Moves = input.next().unwrap().lines().filter_map(|l| {
        let elems : Vec<&str> = l.split(" ").collect();
        if elems.len() != 6 {
            None
        } else {
            Some((elems[1].parse().unwrap(), elems[3].parse::<usize>().unwrap()-1, elems[5].parse::<usize>().unwrap()-1))
        }
    }).collect();

    let n_stacks = (a.last().unwrap().len()+1)/4;
    let mut l : Vec<Vec<char>> = vec![vec![];n_stacks];
    for i in (0..a.len()-1).rev() {
        for j in 0..n_stacks {
            let elem = a[i][j*4+1];
            if elem != ' ' {
                l[j].push(elem);
            }
        }
    }

    star1(l.clone(), &moves);
    star2(l.clone(), &moves);
}
