// use std::collections::HashSet;
use std::collections::HashSet;

type Coord = (isize,isize,isize);

fn volume((x0,y0,z0) : &Coord, (x1,y1,z1) : &Coord) -> isize {
    (x1-x0) * (y1-y0) * (z1-z0)
}

fn volume_act (activated : &Vec<Vec<Vec<bool>>>, range_x : &Vec<isize>, range_y : &Vec<isize>, range_z : &Vec<isize>) -> isize {
    activated.iter().enumerate().map(|(i,a)| {
        a.iter().enumerate().map( |(j,b)| {
            b.iter().enumerate().map( |(k,v)| {
                if *v {
                    let down = (range_x[i], range_y[j], range_z[k]);
                    let up = (range_x[i+1], range_y[j+1], range_z[k+1]);
                    volume(&down,&up)
                } else { 0 }
            }).sum::<isize>()
        }).sum::<isize>()
    }).sum::<isize>()
}

fn stars(s: &Vec<(bool,Coord,Coord)>) {
    let (range_x, xl) : (Vec<isize>, usize) = {
        let limits_x : HashSet<isize> = s.iter().map(|(_,(x0,_,_),(x1,_,_))| [*x0,*x1]).flatten().collect();
        let mut r : Vec<isize> = limits_x.iter().map(|x| *x).collect();
        r.sort();
        let l = r.len();
        (r, l)
    };
    let (range_y, yl) : (Vec<isize>,usize) = {
        let limits_y : HashSet<isize> = s.iter().map(|(_,(_,y0,_),(_,y1,_))| [*y0,*y1]).flatten().collect();
        let mut r : Vec<isize> = limits_y.iter().map(|x| *x).collect();
        r.sort();
        let l = r.len();
        (r, l)
    };
    let (range_z, zl) : (Vec<isize>,usize) = {
        let limits_z : HashSet<isize> = s.iter().map(|(_,(_,_,z0),(_,_,z1))| [*z0,*z1]).flatten().collect();
        let mut r : Vec<isize> = limits_z.iter().map(|x| *x).collect();
        r.sort();
        let l = r.len();
        (r, l)
    };
    let mut activated : Vec<Vec<Vec<bool>>> = vec![vec![vec![false; zl-1]; yl-1]; xl-1];
    let mut init_region = false;
    for (on,(x0,y0,z0),(x1,y1,z1)) in s {
        if !init_region && !((*x0,*y0,*z0) >= (-50,-50,-50)) && !((*x1,*y1,*z1) <= (50,50,50)) {
            println!("{:?}", volume_act(&activated, &range_x, &range_y, &range_z));
            init_region = true;
        }
        for ((i,_i0),_i1) in range_x.iter().enumerate().zip(
            &range_x[1..]).filter(|((_,i0),i1)| *i0 >= x0 && *i1 <= x1) {
            for ((j,_j0),_j1) in range_y.iter().enumerate().zip(
                &range_y[1..]).filter(|((_,j0),j1)| *j0 >= y0 && *j1 <= y1) {
                for ((k,_k0),_k1) in range_z.iter().enumerate().zip(
                    &range_z[1..]).filter(|((_,k0),k1)| *k0 >= z0 && *k1 <= z1) {
                    activated[i][j][k] = *on;
                }
            }
        }
    }
    println!("{:?}", volume_act(&activated, &range_x, &range_y, &range_z));
}

fn parse_input<'a>(s : &'a String) -> Vec<(bool,Coord,Coord)> {
    s.lines().filter_map(|l| {
        if l.len() == 0 { return None }
        let mut i = l.split_whitespace();
        let on = i.next().unwrap() == "on";
        let coords : Vec<(isize,isize)> = i.next().unwrap().split(",").map(
            |c| {
                let mut bounds = c.split("=").skip(1).next().unwrap().split("..");
                (bounds.next().unwrap().parse().unwrap(),
                 // Because it's the upper limit, we add 1 (coordinate)
                 bounds.next().unwrap().parse::<isize>().unwrap()+1)
            }
        ).collect();
        Some((on,(coords[0].0,coords[1].0,coords[2].0),(coords[0].1,coords[1].1,coords[2].1)))
    }).collect()
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    // println!("{:?}", divisions(&(0,0,0),&(3,3,3),&(3,3,3),&(8,8,8)))
    let filename = std::fs::read_to_string(args[1].clone()).unwrap();
    let s: Vec<(bool,Coord,Coord)> = parse_input(&filename);
    stars(&s);
}
