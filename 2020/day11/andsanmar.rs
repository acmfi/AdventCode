use std::fs;

#[derive(PartialEq)]
enum Seat { Floor, Occupied, Empty }

fn seat_people_1<'a>(seats : &Vec<Vec<&'a Seat>>) -> Vec<Vec<&'a Seat>> {
    let get_seat = |(x, y) : (usize, usize)| match seats.get(x) { None => None, Some(s) => s.get(y) };
    let v : Vec<Vec<&Seat>> = (0..seats.len()).map(|x| {
        (0..seats[x].len()).map(|y| {
            let c = [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)].iter().filter(|s| get_seat(**s) == Some(&&Seat::Occupied)).count();
            match seats[x][y] {
                Seat::Empty if c == 0 => &Seat::Occupied,
                Seat::Occupied if c >= 4 => &Seat::Empty,
                s => s
            }
        }).collect()
    }).collect();
    if &v != seats { seat_people_1(&v) } else { v }
}

fn seat_people_2<'a>(seats : &Vec<Vec<&'a Seat>>) -> Vec<Vec<&'a Seat>> {
    let get_seat = |(x, y) : (usize, usize)| match seats.get(x) { None => None, Some(s) => s.get(y) };
    let v : Vec<Vec<&Seat>> = (0..seats.len()).map(|x| {
        (0..seats[x].len()).map(|y| {
            let c : usize = [|x,y,a|(x+a,y),|x,y,a|(x+a,y+a),|x,y,a|(x+a,y-a),|x,y,a|(x-a,y-a),
                             |x,y,a|(x-a,y),|x,y,a|(x-a,y+a),|x,y,a|(x,y-a),|x,y,a|(x,y+a)].iter().fold(
                0, |acc,fun| {
                    struct Rec<'b> { f: &'b dyn Fn(usize, &Rec<'b>) -> usize }
                    let next_seat = Rec { f : &|a,next_seat|  match get_seat(fun(x,y,a)) { Some(&&Seat::Floor) => (next_seat.f)(a+1,next_seat), Some(&&Seat::Occupied) => acc + 1, _ => acc} };
                    (next_seat.f)(1,&next_seat)});
            match seats[x][y] {
                Seat::Empty if c == 0 => &Seat::Occupied,
                Seat::Occupied if c >= 5 => &Seat::Empty,
                s => s
            }
        }).collect()
    }).collect();
    if &v != seats { seat_people_2(&v) } else { v }
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
