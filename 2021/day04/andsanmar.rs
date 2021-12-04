type Line = Vec<(usize,bool)>;
type Board = Vec<Line>;
type Bingo = (Vec<usize>, Vec<Board>);

fn cross(b : &mut Board, n : usize) {
    for l in b {
        for mut e in l {
            if e.0 == n {
                e.1 = true; }}}
}

fn win_board(b : &Board) -> bool {
    b.iter().any(|l| { l.iter().all(|e| e.1) })
}

fn star1((said, t) : &Bingo) {
    let mut tables : Vec<Board> = t.clone();
    for x in said {
        for b in tables.iter_mut() {
            cross(b, *x);
            if win_board(b) || win_board(&transpose(b.clone())) {
                let board_sum = b.iter().map(|l2| l2.iter().map(|e| if !e.1 {e.0} else {0}).sum::<usize>()).sum::<usize>();
                println!("{}", board_sum*x);
                return;
            }
        }
    }
    println!();
}

fn star2((said, t) : &Bingo) {
    let mut tables : Vec<Board> = t.clone();
    let mut completed : Vec<bool> = vec![false; tables.len()];
    for x in said {
        for i in 0..tables.len() {
            let b = tables.get_mut(i).unwrap();
            cross(b, *x);
            if completed[i] == true || win_board(b) || win_board(&transpose(b.clone())) {
                completed[i] = true;
                if completed.iter().all(|x| *x) {
                    let board_sum = b.iter().map(|l2| l2.iter().map(|e| if !e.1 {e.0} else {0}).sum::<usize>()).sum::<usize>();
                    println!("{}", board_sum*x);
                    return;
                }
            }
        }
    }
    println!();
}

fn parse_line(s: &str, del: char) -> Vec<usize> {
    s.split(del).filter_map(|n| match n.parse() {
        Ok(i) => Some(i),
        _ => None
    }).collect()
}


fn transpose<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>> {
    assert!(!v.is_empty());
    let len = v[0].len();
    let mut iters: Vec<_> = v.into_iter().map(|n| n.into_iter()).collect();
    (0..len)
        .map(|_| {
            iters
                .iter_mut()
                .map(|n| n.next().unwrap())
                .collect::<Vec<T>>()
        })
        .collect()
}


fn parse_input(s : String) -> Bingo {
    let mut lines = s.lines();
    let nums : Vec<usize> = parse_line(&s, ',');
    let mut boards : Vec<Board> = vec![];
    lines.next();
    let mut current_board : Vec<Line> = Vec::new();
    while let Some(line) = lines.next() {
        if line.len() == 0 {
            if !current_board.is_empty() { boards.push(current_board); current_board = vec![]; }            
            continue;
        }
        let array: Line = parse_line(line, ' ').iter().map(|x| (*x,false)).collect();
        current_board.push(array);
    }
    if !current_board.is_empty() { boards.push(current_board); }
    (nums, boards)
}


fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let b : Bingo = parse_input(std::fs::read_to_string(args[1].clone()).unwrap());
    star1(&b);
    star2(&b);
}
