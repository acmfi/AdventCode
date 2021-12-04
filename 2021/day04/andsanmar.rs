type Line = Vec<(usize,bool)>;
type Board = Vec<Line>;
type Bingo = (Vec<usize>, Vec<Board>);

fn cross(b : &mut Board, n : usize) {
    for l in b {
        for mut e in l {
            if e.0 == n {
                e.1 = true; }}}
}

fn win_board(b : &Board) -> bool { b.iter().any(|l| { l.iter().all(|e| e.1) }) }

fn stars((said, t) : &Bingo) {
    let mut tables : Vec<Board> = t.clone();
    let mut completed : Vec<bool> = vec![false; tables.len()];
    for x in said {
        for i in 0..tables.len() {
            let b = tables.get_mut(i).unwrap();
            cross(b, *x);
            if completed[i] == true || win_board(b) || win_board(&transpose(b.clone())) {
                if completed.iter().all(|x| !*x) {
                    let board_sum = b.iter().map(|l2| l2.iter().map(|e| if !e.1 {e.0} else {0}).sum::<usize>()).sum::<usize>();
                    println!("{}", board_sum*x);
                }
                completed[i] = true;
                if completed.iter().all(|x| *x) {
                    let board_sum = b.iter().map(|l2| l2.iter().map(|e| if !e.1 {e.0} else {0}).sum::<usize>()).sum::<usize>();
                    println!("{}", board_sum*x);
                    return;
                }}}}
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
    let mut components = s.split("\n\n");
    let nums : Vec<usize> = parse_line(&components.next().unwrap(), ',');
    let boards : Vec<Board> = components.map(|b| {b.lines().map(|line| {
        parse_line(line, ' ').iter().map(|x| (*x,false)).collect()
    }).collect()}).collect();
    (nums, boards)
}


fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let b : Bingo = parse_input(std::fs::read_to_string(args[1].clone()).unwrap());
    stars(&b);
}
