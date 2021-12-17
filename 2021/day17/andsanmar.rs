type Pos = (isize,isize);
type Velocity = (isize,isize);
type Range = ((isize,isize),(isize,isize));

fn falls_in(p : Pos,  v : Velocity, zone : Range) -> bool {
    if (v.0 == 0 && p.0 < zone.0.0) || p.0 > zone.0.1 {
        false
    } else if p.1 < zone.1.0 {
        false
    } else if p.0 >= zone.0.0 && p.0 <= zone.0.1 &&
        p.1 >= zone.1.0 && p.1 <= zone.1.1 {
            true
        }
    else {
        falls_in((p.0+v.0,p.1+v.1), (if v.0 == 0 {0} else {v.0-1}, v.1-1), zone)
    }
}

fn max_height(n : u64) -> u64 { n*(n+1)/2 }

fn stars(zone : Range) {
    let mut in_range = Vec::new();
    for x in 0..=zone.0.1 {
        for y in zone.1.0..-zone.1.0 {
            if falls_in((0,0), (x,y), zone) {
                in_range.push((x,y));
            }
        }
    }
    println!("{}", in_range.iter().map(|n| max_height(n.1 as u64)).max().unwrap());
    println!("{}", in_range.len());
}


fn parse_input<'a>(s : &'a String) -> Range {
    let mut it = s.split(": ");
    let mut range = it.nth(1).unwrap().split(", ");
    let range_x : Vec<isize> = range.next().unwrap()[2..].split("..").map(
        |n| n.parse().unwrap()
    ).collect();
    let range_y : Vec<isize> = range.next().unwrap()[2..].split("..").map(
        |n| n.parse().unwrap()
    ).collect();
    ((range_x[0], range_x[1]),(range_y[0], range_y[1]))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let filename = std::fs::read_to_string(args[1].clone()).unwrap();
    let s: Range = parse_input(&filename);
    stars(s);
}
