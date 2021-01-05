use std::fs;
use std::collections::HashMap;

#[derive(PartialEq,Eq,Hash,PartialOrd,Ord)]
struct Coord(isize,isize,isize,isize);

fn neighbors(c: &Coord, include_original: bool) -> Vec<Coord> {
    (c.0-1..c.0+2).map(|x : isize| {
        (c.1-1..c.1+2).map(|y| {
            (c.2-1..c.2+2).map(|z| {
                (c.3-1..c.3+2).map(|w| {
                    Coord(x,y,z,w)
                }).filter(|coord| include_original || coord != c).collect::<Vec<Coord>>()
            }).flatten().collect::<Vec<Coord>>()
        }).flatten().collect::<Vec<Coord>>()
    }).flatten().collect::<Vec<Coord>>()
}

fn neighbors_active(c : &Coord, grid: &HashMap<Coord,bool>) -> usize {
    neighbors(c, false).iter().fold(0, |acc,n| if grid.get(&n) == Some(&true) { acc+1 } else { acc} )
}

fn cycle(grid: &HashMap<Coord,bool>) -> HashMap<Coord,bool> {
    let mut new_grid : HashMap<Coord,bool> = HashMap::new();
    let mut to_check : Vec<Coord> = grid.keys().map(|c| neighbors(c, true)).flatten().collect();
    to_check.sort();
    to_check.dedup();
    for c in to_check {
        let act = neighbors_active(&c, grid);
        if act == 3 {
            new_grid.insert(c, true);
        } else if grid.get(&c) == Some(&true) && act == 2 {
            new_grid.insert(c, true);
        }
    }
    new_grid
}

fn main () {
    let r = fs::read_to_string("input").unwrap();
    let mut grid : HashMap<Coord,bool> = HashMap::new();
    let initial_active = r.lines().enumerate().map(|(i,l)| (i,l.chars().enumerate().filter(|(_,e)| e == &'#')));
    for (x,l) in initial_active { for (y,_) in l { grid.insert(Coord(x as isize,y as isize,0,0), true); }}

    for _ in 0..6 {
        println!("{}", grid.values().len());
        grid = cycle(&grid);
    }
    println!("{}", grid.values().len());
}
