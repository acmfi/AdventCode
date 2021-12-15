use std::collections::HashSet;

fn star1(s: &Vec<Vec<u16>>) {
    let mut visited : HashSet<(usize,usize)> = HashSet::new();
    let mut adjacents : HashSet<(usize,usize)> = HashSet::new();
    let x_d = s.len();
    let y_d = s[0].len();

    // Distances from starting point, updated while visiting new points
    let mut m : Vec<Vec<Option<u16>>> = vec![vec![None;s.len()];s.len()];

    // Initaliziation phase
    m[0][0] = Some(0);
    m[1][0] = Some(s[1][0]);
    adjacents.insert((1,0));
    m[0][1] = Some(s[0][1]);
    adjacents.insert((0,1));

    // We track the sum of risks until we reach the end point
    while m[x_d-1][y_d-1] == None {
        let (i,j) = adjacents.iter().filter_map(
            |(x,y)| {
                if *x < x_d && *y < y_d {
                    match m[*x][*y] {
                        Some(_) => Some((*x,*y)),
                        None => None
                    }                    
                } else {
                    None
                }
            }
        ).min_by(
            |p1,p2| m[p1.0][p1.1].cmp(&m[p2.0][p2.1])
        ).unwrap();
        visited.insert((i,j));
        adjacents.remove(&(i,j));

        // We consider the new adjacents if they have not been visited
        for (x,y) in [(i-1,j),(i+1,j),(i,j-1),(i,j+1)] {
            if x < x_d && y < y_d && !visited.contains(&(x,y)) {
                adjacents.insert((x,y));
                let new_value = m[i][j].unwrap() + s[x][y];
                match m[x][y] {
                    Some(n) => if n > new_value {
                        m[x][y] = Some(new_value);
                    },
                    None => {
                        m[x][y] = Some(new_value);
                    }
                }
            }
        }
    }
    println!("{}", m[x_d-1][y_d-1].unwrap());
}


fn parse_input<'a>(s : &'a String) -> Vec<Vec<u16>> {
    s.lines().filter_map(|l| {
        let r : Vec<u16> = l.chars().filter_map(
            |c| match c.to_digit(10) {
                Some(n) => Some(n as u16),
                _ => None
            }).collect();
        match r.len() {
            0 => None,
            _ => Some(r)
        }
    }).collect()
}

fn new_map(m : Vec<Vec<u16>>) -> Vec<Vec<u16>> {
    let dim = m.len();
    let mut map : Vec<Vec<u16>> = vec![vec![0;m.len()*5];m[0].len()*5];
    for x in 0..map.len() {
        for y in 0..map[0].len() {
            let n = m[x%dim][y%dim] + (x/dim + y/dim) as u16;
            map[x][y] = if n >= 10 { n%10 + 1} else {n};
        }
    }
    map
}


fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let filename = std::fs::read_to_string(args[1].clone()).unwrap();
    let s: Vec<Vec<u16>> = parse_input(&filename);
    star1(&s);
    let new_map : Vec<Vec<u16>> = new_map(s);
    // for l in &new_map {
    //     println!("{:?}", l);
    // }
    star1(&new_map);
}
