type Coord = (i64,i64);
type Struct = Vec<(Coord,i64)>; // Center + Radius

fn merge(s : &Vec<(i64,i64)>) -> Vec<(i64, i64)> {
    let (mut cl, mut cr) = s[0];
    let mut v = vec![];
    for (l,r) in &s[1..] {
        if *l <= cr {
            cr = if *r <= cr {
                cr
            } else {
                *r
            }
        } else {
            v.push((cl,cr));
            (cl,cr) = (*l,*r);
        }
    }
    v.push((cl,cr));
    v
}

fn row_stats(l : &Struct, row : i64) -> Vec<(i64,i64)> {
    let mut overlaps = Vec::new();
    for ((x,y),rad) in l {
        if row >= y-rad && row <= y+rad { // It overlaps with the row
            if row < *y { // TOP
                let dif = ((y-rad) - row).abs();
                // println!("{:?}", (((x,y),rad),(x-dif,x+dif)));
                overlaps.push((x-dif,x+dif));
            } else if row > *y { // BOTTOM
                let dif = ((y+rad) - row).abs();
                // println!("{:?}", (((x,y),rad),(x-dif,x+dif)));
                overlaps.push((x-dif,x+dif));
            }
        }
    }
    overlaps.sort();
    let m = merge(&overlaps);
    m
}

fn star1(l : &Struct) {
    println!("{:?}", row_stats(&l, 2000000).iter().map(|(x,y)| (x-y).abs()).sum::<i64>());
}

fn star2(l : &Struct) {
    let mut found = (0,0);
    for y in 0..4000000 {
        let u = row_stats(l, y);
        for ((_,right),(next_left,_)) in u.iter().zip(u.iter().skip(1)) {
            // This may fail, there could be adjacent points in the vertical axises
            if right+1 > 0 && right+1 < 4000000 && *next_left == right+2 {
                found = (right+1,y);
            }
        }
    }
    println!("{:?}", found.0*4000000+found.1);
}

fn parse(input: &str) -> Option<(Coord,i64)> {
    let mut e = input.split_whitespace();
    let x1 : Result<i64,_> = e.nth(2)?.split("=").nth(1)?.split(",").next()?.parse();
    let y1 : Result<i64,_> = e.next()?.split("=").nth(1)?.split(":").next()?.parse();
    let x2 : Result<i64,_> = e.nth(4)?.split("=").nth(1)?.split(",").next()?.parse();
    let y2 : Result<i64,_> = e.next()?.split("=").nth(1)?.parse();
    match (x1,y1,x2,y2) {
        (Ok(x),Ok(y),Ok(i),Ok(j)) => Some(((x,y),(i-x).abs()+(j-y).abs())),
        _ => None
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let l : Struct = std::fs::read_to_string(args[1].clone()).unwrap().lines().filter_map(|l| {
        if !l.is_empty() {
            parse(l)
        }
        else { None }
    }).collect();

    star1(&l);
    star2(&l);
}
