use std::fs;

#[derive(Debug)]
enum Dir {
    East, SouthEast, NorthEast,
    West, SouthWest, NorthWest
}

fn coord(v: &Dir) -> (isize, isize) {
    match v {
        Dir::East => (1, 0),
        Dir::West => (-1, 0),
        Dir::NorthEast => (0, -1),
        Dir::NorthWest => (-1, -1),
        Dir::SouthEast => (1, 1),
        Dir::SouthWest => (0, 1),
    }
}

fn area_tile((x,y) : (isize,isize)) -> [(isize,isize);7] {[(x,y),(x+1,y),(x-1,y),(x,y-1),(x-1,y-1),(x+1,y+1),(x,y+1)]}

fn automaton(floor_blacks: &Vec<(isize,isize)>) -> Vec<(isize,isize)> {
    let mut all_tiles : Vec<(isize,isize)> = floor_blacks.iter().map(|t| area_tile(*t).to_vec()).flatten().collect();
    all_tiles.sort();
    all_tiles.dedup();
    let new_blacks : Vec<(isize,isize)> = all_tiles.iter().filter(|c| {
        let adjs : &[(isize,isize)] = &area_tile(**c)[1..];
        let adj_black = adjs.iter().filter(|x| floor_blacks.contains(x)).count();
        if floor_blacks.contains(&c) {
            adj_black == 1 || adj_black == 2
        } else {
            adj_black == 2
        }
    }).map(|x| *x).collect();
    new_blacks
}

fn main() {
    let r: String = fs::read_to_string("input").unwrap();
    let t = r.lines().map(|l| {
        l.chars().fold((Vec::new(),None), |(mut tiles,mut ns),c| {
            match ns {
                None => {match c {
                    'e' => tiles.push(Dir::East),
                    'w' => tiles.push(Dir::West),
                    'n' => ns = Some(true),
                    's' => ns = Some(false),
                    _ => panic!()
                }; (tiles,ns)},
                Some(true) => {tiles.push(match c {
                    'e' => Dir::NorthEast,
                    'w' => Dir::NorthWest,
                    _ => panic!()
                }); (tiles,None)},
                Some(false) => {tiles.push(match c {
                    'e' => Dir::SouthEast,
                    'w' => Dir::SouthWest,
                    _ => panic!()
                }); (tiles,None)},
            }
        }).0
    });

    let tiles_coords: Vec<(isize, isize)> = t.map(|dirs| {
        dirs.iter().fold((0, 0), |(cx, cy), d| {
            let (sx, sy) = coord(d);
            (cx + sx, cy + sy)
        })}).collect();

    let blacks: Vec<(isize, isize)> = tiles_coords.iter().filter(|c| tiles_coords.iter().filter(|x| x == c).count() % 2 == 1).map(|x| *x).collect();
    
    println!("{}", blacks.len());

    let star2 = (0..100).fold(blacks, |acc,_| automaton(&acc));
    println!("{}", star2.len());
}
