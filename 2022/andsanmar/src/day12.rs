use std::collections::HashSet;

type Struct = Vec<Vec<u64>>;

fn find(c: char,  l : &Struct) -> (usize, usize){
    let code = c as u64;
    for i in 0..l.len() {
        for j in 0..l[i].len() {
            if l[i][j] == code {
                return (i,j)
            }
        }
    }
    (0,0)
}

fn update_elems((i,j): (usize, usize), vals: &mut Vec<Vec<Option<u64>>>, l: &Struct) -> Vec<(usize,usize)> {
    let mut coords = vec![(i+1,j),(i,j+1)];
    if i!=0 { coords.push((i-1,j)); }
    if j!=0 { coords.push((i,j-1)); }

    let mut updated = vec![];

    for (x,y) in coords {
        if let Some(v) = l.get(x) {
            if let Some(e) = v.get(y) {
                if e-1 <= l[i][j] {
                    if vals[x][y] == None || vals[x][y] > vals[i][j] {
                        vals[x][y] = Some(vals[i][j].unwrap() + 1);
                        updated.push((x,y))
                    }
                }
            }
        }
    }
    updated
}

fn path(start : (usize, usize), end : (usize, usize), l : &Struct) -> Option<u64> {
    let mut vals = vec![vec![None;l[0].len()];l.len()];
    vals[start.0][start.1] = Some(0);
    let mut to_inspect = HashSet::from([start]);

    while vals[end.0][end.1] == None {
        let updated = to_inspect.iter().map(|coord| update_elems(*coord, &mut vals, l)).flatten().collect();
        to_inspect = updated;
        if to_inspect.is_empty() {
            break
        }
    }

    vals[end.0][end.1]
}

fn stars(l : &mut Struct) {
    let start = find('S', &l);
    l[start.0][start.1] = 'a' as u64;
    let end = find('E', &l);
    l[end.0][end.1] = 'z' as u64;

    let l : &Struct = l;
    println!("{}", path(start, end, l).unwrap()); // star1
    let vals = (0..l.len()).map(|i| {
        (0..l[i].len()).filter_map(move |j| {
            if l[i][j] == 'a' as u64 {
                path((i,j), end, l)
            } else {
                None
            }
        })
    }).flatten();
    println!("{}", vals.min().unwrap());
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let mut l : Struct = std::fs::read_to_string(args[1].clone()).unwrap().lines().filter_map(|l| {
        if !l.is_empty() {
            Some(l.chars().map(|c| c as u64).collect())
        }
        else { None }
    }).collect();

    stars(&mut l);
}
