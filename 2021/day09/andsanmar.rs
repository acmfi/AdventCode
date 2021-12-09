use std::collections::HashSet;

fn adjs((i,j): (usize,usize), s : &Vec<Vec<usize>>) -> Vec<((usize,usize),usize)> {
    let mut r : Vec<((usize,usize),usize)> = vec![];
    if let Some(v) = s.get(i-1) {
        r.push(((i-1,j),v[j]));
    }
    if let Some(e) = s[i].get(j-1) {
        r.push(((i,j-1),*e));
    }
    if let Some(e) = s[i].get(j+1) {
        r.push(((i,j+1),*e));
    }
    if let Some(v) = s.get(i+1) {
        r.push(((i+1,j),v[j]));
    }
    r
}

fn basin((i,j): (usize,usize), s : &Vec<Vec<usize>>) -> HashSet<(usize,usize)> {
    let mut higher : HashSet<(usize,usize)> = HashSet::new();
    let low = s[i][j];
    for (c,_) in adjs((i,j), s).iter().filter(|(_,e)| *e > low  && *e !=9) {
        higher.insert(*c);
        higher.extend(&basin(*c, s));
    }
    higher
}

fn stars(s : &Vec<Vec<usize>>) {
    let mut low_points : Vec<(usize,usize)> = vec![];
    for i in 0..s.len() {
        for j in 0..s[i].len() {
            let low = s[i][j];
            if adjs((i,j), s).iter().all(|(_,e)| *e > low) {
                low_points.push((i,j));
            }
        }
    }
    println!("{}", low_points.iter().map(|x| s[x.0][x.1]+1).sum::<usize>());
    let mut basins : Vec<usize> = low_points.iter().map(|p| basin(*p, s).len()+1).collect();
    basins.sort_by(|a,b| b.cmp(a));
    println!("{:?}", basins[0..3].iter().product::<usize>());
}

fn parse_input(s : String) -> Vec<Vec<usize>> {
    s.lines().map(|l| {
        l.chars().filter_map(|n| match n.to_digit(10) {
            Some(i) => Some(i as usize),
            _ => None
        }).collect()
    }).collect()
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let s: Vec<Vec<usize>> = parse_input(std::fs::read_to_string(args[1].clone()).unwrap());
    stars(&s);
}
