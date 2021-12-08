use std::collections::HashSet;
#[derive(Debug,Copy,Clone,PartialOrd,PartialEq,Eq,Hash)]
enum Segment { A,B,C,D,E,F,G }
type Wires = HashSet<Segment>;
type Display = Vec<(Vec<Wires>,Vec<Wires>)>;

fn star1(d : &Display) {
    println!("{}", d.iter().map(|(_,s)| s.iter().filter(|c| match c.len() {
        2|3|4|7 => true, _ => false
    }).count()).sum::<usize>());
}

fn star2(d : &Display) {
    let sum_out : usize = d.iter().map(|(i,e)| {
        let one = i.iter().find(|n| n.len() == 2).unwrap();
        let seven = i.iter().find(|n| n.len() == 3).unwrap();
        let four = i.iter().find(|n| n.len() == 4).unwrap();
        let eight = i.iter().find(|n| n.len() == 7).unwrap();
        let three = i.iter().find(|n| n.len() == 5 && seven.is_subset(&n)).unwrap();
        let nine = i.iter().find(|n| n.len() == 6 && four.is_subset(&n)).unwrap();
        let zero = i.iter().find(|n| n.len() == 6 && seven.is_subset(&n) && n != &nine).unwrap();
        let six = i.iter().find(|n| n.len() == 6 && n != &nine && n != &zero).unwrap();
        let five = i.iter().find(|n| n.len() == 5 && n.is_subset(six)).unwrap();
        let two = i.iter().find(|n| n.len() == 5 && n != &three && n != &five).unwrap();
        e.iter().map(|x| {
            if x == one {'1'}
            else if x == two { '2' }
            else if x == three { '3' }
            else if x == four { '4' }
            else if x == five { '5' }
            else if x == six { '6' }
            else if x == seven { '7' }
            else if x == eight { '8' }
            else if x == nine { '9' }
            else if x == zero { '0' }
            else { ' ' }
        }).collect::<String>().parse::<usize>().unwrap()
    }).sum();
    println!("{}", sum_out);
}

fn parse_input(s : String) -> Display {
    s.lines().filter_map(|l| {
        let wires : Vec<Vec<Wires>> = l.split("|").map(|c| c.trim().split(" ").map(|w| {
            w.chars().filter_map(|ch| match ch {
                'a' => Some(Segment::A),
                'b' => Some(Segment::B),
                'c' => Some(Segment::C),
                'd' => Some(Segment::D),
                'e' => Some(Segment::E),
                'f' => Some(Segment::F),
                'g' => Some(Segment::G),
                _ => None
            }).collect()
        }).collect()).collect();
        if wires.len() != 2 { None } else {Some((wires[0].clone(), wires[1].clone()))}
    }).collect()
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let s: Display = parse_input(std::fs::read_to_string(args[1].clone()).unwrap());
    star1(&s);
    star2(&s);
}
