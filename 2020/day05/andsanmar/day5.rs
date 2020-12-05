use std::fs;

fn main() {
    let r = fs::read_to_string("input").unwrap();
    let mut passes_ids : Vec<u16> = r.lines().map(|l| {
        u16::from_str_radix(l.chars().map(|c| match c {
            'B' | 'R' => '1',
            _ => '0'
        }).collect::<String>().as_str(), 2).unwrap()
    }).collect();
    passes_ids.sort();
    println!("{:?}", passes_ids.iter().last());
    for x in passes_ids[0].. {
        if ! passes_ids.contains(&x) {
            println!("{:?}", x);
            break
        }
    }
}
