use std::fs;

fn star1((min, max, c, s) : (usize, usize, char, &str)) -> bool {
    let o = s.chars().filter(|x| *x == c).count();
    o <= max && o >= min
}

fn star2((min, max, c, s) : (usize, usize, char, &str)) -> bool {
    (s.chars().nth(min-1).unwrap() == c) ^ (s.chars().nth(max-1).unwrap() == c)
}

fn convert(s : &str) -> (usize, usize, char, &str) {
    let sp : Vec<&str> = s.split_whitespace().collect();
    let minmax : Vec<usize> = sp[0].split('-').map(|x| x.parse::<usize>().unwrap()).collect();
    (minmax[0],minmax[1],sp[1].chars().next().unwrap(),sp[2])
}

fn main() {
    let r = fs::read_to_string("input").unwrap();
    let pass : Vec<(usize, usize, char, &str)> = r.lines().map(|x| convert(x)).collect();
    println!("{}", pass.iter().filter(|t| star1(**t)).count());
    println!("{}", pass.iter().filter(|t| star2(**t)).count());
}
