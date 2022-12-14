use std::collections::HashSet;

type Struct<'a> = HashSet<(usize,usize)>;

fn next((x,y) : (usize, usize)) -> [(usize,usize);3] {
    [(x,y+1),(x-1,y+1),(x+1,y+1)]
}

fn stars(mut l : Struct) {
    let to_void = l.iter().max_by(|x, y| x.1.cmp(&y.1)).unwrap().1;
    for i in 0..1000 { // Floor, for star2
        l.insert((i,to_void+2));
    }

    let mut star1 = false;
    let wall_size = l.len();
    
    loop {
        let mut sand = (500,0);
        while star1 || sand.1 <= to_void {
            let mut falling : bool = false;
            for el in next(sand) {
                if !falling && !l.contains(&el) {
                    sand = el;
                    falling = true;
                    break;
                }
            }
            if !falling {
                l.insert(sand);
                break;
            }
        }
        if !star1 && sand.1 > to_void {
            star1 = true;
            println!("{:?}", l.len()-wall_size);
        }
        if sand == (500,0) { // Star2
            break;
        }
    }
    println!("{:?}", l.len()-wall_size);
}

fn parse(input : &str) -> Vec<(usize,usize)> {
    let pos : Vec<(usize,usize)> = input.split(" -> ").filter_map(|e| {
        let mut el = e.split(",");
        Some((el.next().unwrap().parse().unwrap(), el.next().unwrap().parse().unwrap()))
    }).collect();
    let mut grid = Vec::new();
    for ((x,y),(i,j)) in pos.iter().zip(&pos[1..]) {
        if x == i {
            let min = y.min(j);
            let max = y.max(j);
            for k in *min..=*max {
                grid.push((*x,k));
            }
        } else if y == j {
            let min = x.min(i);
            let max = x.max(i);
            for k in *min..=*max {
                grid.push((k,*y));
            }
        } else {
            panic!("Not straight lines!");
        }
    }
    grid
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Struct = std::fs::read_to_string(args[1].clone()).unwrap().lines().filter_map(|l| {
        if !l.is_empty() {
            Some(parse(l))
        }
        else { None }
    }).flatten().collect();

    stars(l);
    // star2(l);
}
