use std::fs;

#[derive(Debug, PartialEq)] // , Eq, Copy, Clone
enum Seat { Floor, Occupied, Empty }

fn seat_people_1<'a>(seats : &Vec<Vec<&'a Seat>>) -> Vec<Vec<&'a Seat>> {
    let mut v : Vec<Vec<&Seat>> = Vec::new();
    for x in 0..seats.len() {
        let mut row : Vec<&Seat> = Vec::new();
        for y in 0..seats[x].len() { // TODO maybe better to do a zip?
            let mut adjacent : Vec<&Seat> = Vec::new();
            if x > 0 { adjacent.push(seats[x-1][y]);
            if y > 0 { adjacent.push(seats[x-1][y-1]) }
            if y < seats[x].len()-1 { adjacent.push(seats[x-1][y+1]) }}
            if x < seats.len()-1 { adjacent.push(seats[x+1][y]);
            if y > 0 { adjacent.push(seats[x+1][y-1]) }
            if y < seats[x].len()-1 { adjacent.push(seats[x+1][y+1]) } }
            if y > 0 { adjacent.push(seats[x][y-1]) }
            if y < seats[x].len()-1 { adjacent.push(seats[x][y+1]) }
            let c = adjacent.iter().filter(|s| *s == &&Seat::Occupied).count();
            row.push( match seats[x][y] {
                Seat::Empty if c == 0 => &Seat::Occupied,
                Seat::Occupied if c >= 4 => &Seat::Empty,
                s => s
            });
        }
        v.push(row);
    }
    if &v != seats {
        seat_people_1(&v)
    } else { v }
}

fn seat_people_2<'a>(seats : &Vec<Vec<&'a Seat>>) -> Vec<Vec<&'a Seat>> {
    let mut v : Vec<Vec<&Seat>> = Vec::new();
    for x in 0..seats.len() {
        let mut row : Vec<&Seat> = Vec::new();
        for y in 0..seats[x].len() {
            let mut adjacent : Vec<&Seat> = Vec::new();
            for cx in (0..x).rev() { if seats[cx][y] != &Seat::Floor { adjacent.push(seats[cx][y]); break}}
            for cx in (x+1)..(seats.len()) { if seats[cx][y] != &Seat::Floor { adjacent.push(seats[cx][y]); break}}
            for cy in (0..y).rev() { if seats[x][cy] != &Seat::Floor { adjacent.push(seats[x][cy]); break}}
            for cy in (y+1)..(seats[x].len()) { if seats[x][cy] != &Seat::Floor { adjacent.push(seats[x][cy]); break}}
            for a in 1.. {
                if x+a >= seats.len() || y+a >= seats[x].len() {break;} else if
                    seats[x+a][y+a] != &Seat::Floor { adjacent.push(seats[x+a][y+a]); break}}
            for a in 1.. {
                if x-a >= seats.len() || y-a >= seats[x].len() {break;} else if
                    seats[x-a][y-a] != &Seat::Floor { adjacent.push(seats[x-a][y-a]); break}}
            for a in 1.. {
                if x+a >= seats.len() || y-a >= seats[x].len() {break;} else if
                    seats[x+a][y-a] != &Seat::Floor { adjacent.push(seats[x+a][y-a]); break}}
            for a in 1.. {
                if x-a >= seats.len() || y+a >= seats[x].len() {break;} else if
                    seats[x-a][y+a] != &Seat::Floor { adjacent.push(seats[x-a][y+a]); break}}
            let c = adjacent.iter().filter(|s| *s == &&Seat::Occupied).count();
            row.push( match seats[x][y] {
                Seat::Empty if c == 0 => &Seat::Occupied,
                Seat::Occupied if c >= 5 => &Seat::Empty,
                s => s
            });
        }
        v.push(row);
    }
    if &v != seats {
        seat_people_2(&v)
    } else { v }
}


fn main () {
    let r = fs::read_to_string("input").unwrap();
    let seats : Vec<Vec<&Seat>> = r.lines().map(|l| l.chars().map(|s| match s {
        '#' => &Seat::Occupied,
        'L' => &Seat::Empty,
        '.' => &Seat::Floor,
        _ => panic!()
    }).collect()).collect();
    let fixpoint_1 = seat_people_1(&seats);
    println!("{:?}", fixpoint_1.iter().flatten().filter(|s| *s == &&Seat::Occupied).count());
    let fixpoint_2 = seat_people_2(&seats);
    println!("{:?}", fixpoint_2.iter().flatten().filter(|s| *s == &&Seat::Occupied).count());
}
