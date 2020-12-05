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
    println!("{:?}", passes_ids.last());
    let c = passes_ids[0];
    for (x, n) in passes_ids.iter().enumerate() {
        if n - x as u16 != c {
            println!("{:?}", n-1); // The previous entry was missing
            break
        }
    }
}
