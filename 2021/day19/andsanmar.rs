use std::collections::HashMap;

type Scanner = Vec<(isize,isize,isize)>;

fn perm ((x,y,z) : (isize, isize, isize)) -> Vec<(isize,isize,isize)> {
    let mut r = vec![];
    for sx in [-1,1] {
        for sy in [-1,1] {
            for sz in [-1,1] {
                r.push((sx*x,sy*y,sz*z));
                r.push((sx*x,sz*z,sy*y));
                r.push((sy*y,sz*z,sx*x));
                r.push((sy*y,sx*x,sz*z));
                r.push((sz*z,sy*y,sx*x));
                r.push((sz*z,sx*x,sy*y));
            }   
        }         
    }
    r
}


// Match happens when there is
fn stars(s: Vec<Scanner>) {
    let mut positions : HashMap<(isize,isize,isize),u8> = s[0].iter().map(|p| (*p,1)).collect();
    let mut scanners : Vec<(isize,isize,isize)> = vec![(0,0,0)];
    let mut pending : Vec<usize> = (1..s.len()).collect();

    while let Some(k) = pending.pop() {
        let scanner = s.get(k).unwrap();
        let perms : Vec<Scanner>  = scanner.iter().map(|e| perm(*e)).collect();
        let l = perms[0].len();
        let mut possible_locations : Vec<HashMap<(isize,isize,isize),u8>> =  vec![HashMap::new();l];
        for j in 0..l {
            for i in 0..perms.len() {
                let e2 = perms[i][j];
                for e1 in positions.keys() {
                    let pos = (e1.0+e2.0,e1.1+e2.1,e1.2+e2.2);
                    match possible_locations[j].get_mut(&pos) {
                        Some(n) => *n += 1,
                        None => {let _= possible_locations[j].insert(pos, 1);}
                    }
                }
            }
        }
        let mut scanner_position : Option<((isize,isize,isize),usize)> = None;
        for index_permutation in 0..l { // possible_locations {
            match possible_locations.get(index_permutation).unwrap().iter().max_by(|(_,a),(_,b)| a.cmp(b)) {
                Some((possible_scanner,coincidences)) => {
                    if *coincidences >= 12 {
                        scanner_position = Some((*possible_scanner,index_permutation))
                    }
                },
                None => {}
            }
        }
        match scanner_position {
            Some((scanner,index)) => {
                scanners.push(scanner);
                for p in perms {
                    let offset = p[index];
                    let pos = (scanner.0-offset.0,scanner.1-offset.1,scanner.2-offset.2);
                    match positions.get_mut(&pos) {
                        Some(n) => *n += 1,
                        None => {let _= positions.insert(pos, 1);}
                    }
                }
            },
            None => {pending.insert(0,k)}
        }
    }
    println!("{:?}", positions.len()); // Star 1
    
    let mut distances = vec![];
    for s1 in &scanners {
        for s2 in &scanners {
            distances.push((s1.0-s2.0).abs() + (s1.1-s2.1).abs() + (s1.2-s2.2).abs());
        }
    }
    println!("{:?}", distances.iter().max().unwrap()); // Star 2
}

fn parse_input<'a>(s : &'a String) -> Vec<Scanner> {
    let scanners : Vec<&str> = s.split("\n\n").collect();
    scanners.iter().filter_map(|sc| {
        if sc.len() != 0 {
            Some(sc.lines().skip(1).filter_map(|l| {
                let v : Vec<isize> = l.split(",").filter_map(|n| {
                    match n.parse::<isize>() {
                        Ok(e) => Some(e),
                        _ => None
                    }
                }).collect();
                if v.len() == 3 {
                    Some((v[0], v[1], v[2]))
                } else {
                    None
                }
            }).collect())
        } else {
            None
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
    let s: Vec<Scanner> = parse_input(&filename);
    stars(s);
}
