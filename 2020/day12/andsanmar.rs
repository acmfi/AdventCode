use std::fs;

#[derive(Debug, PartialEq)]
enum Dir {
    North, South, East, West
}
#[derive(Debug, PartialEq)]
enum Turn {
    Left, Right, Forward
}

#[derive(Debug, PartialEq)]
enum Action<'a> { Dir(&'a Dir), Turn(&'a Turn)}

fn turn<'a>(prev: &'a Dir, new: &'a Turn, degrees: isize) -> &'a Dir {
    let dir = match new {
        &Turn::Forward => return prev,
        &Turn::Left => match prev {
            &Dir::North => &Dir::West,
            &Dir::West => &Dir::South,
            &Dir::South => &Dir::East,
            &Dir::East => &Dir::North
        },
        &Turn::Right => match prev {
            &Dir::North => &Dir::East,
            &Dir::East => &Dir::South,
            &Dir::South => &Dir::West,
            &Dir::West => &Dir::North
        },
    };
    if degrees == 90 { dir } else { turn(dir, new, degrees - 90) }
}

#[derive(Debug)]
struct Coords (isize, isize);

fn move_ship<'a>(s: Coords, dir: &'a Dir, (action, moved): &(&'a Action, isize)) -> (Coords, &'a Dir) {
    let (new_dir, move_towards) : (&Dir, &Dir) = match action {
        Action::Dir(d) => (&dir, d),
        Action::Turn(t) => { let d = turn(&dir, t, *moved); (d, d)}
    };
    if &&Action::Turn(&Turn::Right) == action || &&Action::Turn(&Turn::Left) == action {
        return (s, new_dir)
    }
    (match move_towards {
        &Dir::East => Coords(s.0+moved, s.1),
        &Dir::West => Coords(s.0-moved, s.1),
        &Dir::North => Coords(s.0, s.1+moved),
        &Dir::South => Coords(s.0, s.1-moved)
    }, new_dir)
}

fn star2(s: Coords, w : Coords, (action, moved): &(&Action, isize)) -> (Coords, Coords) {
    match action {
        &Action::Dir(&Dir::North) => (s, Coords(w.0, w.1 + moved)),
        &Action::Dir(&Dir::East) => (s, Coords(w.0 + moved, w.1)),
        &Action::Dir(&Dir::West) => (s, Coords(w.0 - moved, w.1)),
        &Action::Dir(&Dir::South) => (s, Coords(w.0, w.1 - moved)),
        &Action::Turn(&Turn::Forward) => (Coords(s.0+w.0*moved, s.1+w.1*moved),w),
        &Action::Turn(&Turn::Right) => if moved == &0 {(s,w)} else {
            star2(s,Coords(w.1,-w.0),&(action, *moved-90))},
        &Action::Turn(&Turn::Left) => if moved == &0 {(s,w)} else {
            star2(s,Coords(-w.1,w.0),&(action, *moved-90))},
    }
}

fn main () {
    let r = fs::read_to_string("input").unwrap();
    let actions : Vec<(&Action, isize)> = r.lines().map( |l| {
        let mut it = l.chars();
        (match it.next().unwrap() {
            'L' => &Action::Turn(&Turn::Left),
            'R' => &Action::Turn(&Turn::Right),
            'F' => &Action::Turn(&Turn::Forward),
            'S' =>  &Action::Dir(&Dir::South),
            'E' =>  &Action::Dir(&Dir::East),
            'W' =>  &Action::Dir(&Dir::West),
            'N' =>  &Action::Dir(&Dir::North),
            _ => &Action::Dir(&Dir::North)
        }, it.as_str().parse::<isize>().unwrap())
    }).collect();
    let (ship1,_) : (Coords,&Dir) = actions.iter().fold(
        (Coords(0,0), &Dir::East), |(s,d),a| move_ship(s,d,a));
    println!("{}", ship1.0.abs() + ship1.1.abs());
    let (ship2, _) : (Coords, Coords) = actions.iter().fold(
        (Coords(0,0), Coords(10,1)), |(s,w),a| star2(s,w,a));
    println!("{}", ship2.0.abs()+ship2.1.abs());
}
