use std::collections::HashMap;

fn star1(s: &Vec<Vec<u16>>) {
    // Distances from starting point, updated while visiting new points
    let mut visited : HashMap<(usize,usize),u16> = HashMap::new();
    let mut adjacents : HashMap<(usize,usize),u16> = HashMap::new();
    let x_d = s.len();
    let y_d = s[0].len();

    // Initaliziation phase
    visited.insert((0,0),0);
    adjacents.insert((1,0),s[1][0]);
    adjacents.insert((0,1),s[0][1]);

    // We track the sum of risks until we reach the end point
    while visited.get(&(x_d-1,y_d-1)) == None {
        let l = adjacents.iter().min_by(
            |(_,n1),(_,n2)| n1.cmp(&n2)
        ).unwrap();
        let ((i,j),n) = (*l.0,*l.1);
        visited.insert((i,j), n);
        adjacents.remove(&(i,j));

        // We consider the new adjacents if they have not been visited
        for (x,y) in [(i-1,j),(i+1,j),(i,j-1),(i,j+1)] {
            if x < x_d && y < y_d && visited.get(&(x,y)) == None {
                let new_value = visited.get(&(i,j)).unwrap() + s[x][y];
                let old_value = adjacents.get(&(x,y));
                let v = match old_value {
                    None => new_value,
                    Some(ov) => if *ov < new_value {
                        *ov
                    } else {
                        new_value
                    }
                };
                adjacents.insert((x,y),v);
            }
        }
    }
    println!("{}", visited.get(&(x_d-1,y_d-1)).unwrap());
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
