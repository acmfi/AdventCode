use std::fs;

// const NUMS : u32 = 10;
const NUMS : i32 = 10;

fn ad(n: i32) -> u32 { if (n as i32) <= 0 {(NUMS-1+n) as u32 } else {n as u32} }

fn move_cups(cups: &Vec<u32>) -> Vec<u32> {
    let c = cups[0];
    let sel : &[u32] = &cups[1..4];

    let to_place = cups.iter().enumerate().find(|(_,n)| **n == ad(c as i32 - match sel.contains(&ad(c as i32-1)) {
        false => 1,
        true => match sel.contains(&ad(c as i32-2)) {
            false => 2,
            true => match sel.contains(&ad(c as i32-3)) {
                false => 3,
                true => 4
            }
        }
    })).unwrap().0;
    let mut new_vec = cups[4..to_place+1].to_vec();
    new_vec.extend_from_slice(&sel);
    new_vec.extend_from_slice(&cups[to_place+1..]);
    new_vec.push(c);
    new_vec
}

fn main () {
    let mut n = fs::read_to_string("input").unwrap().chars().map(|e| e.to_digit(10).unwrap()).collect::<Vec<u32>>();
    // for x in 10..NUMS {
    //     n.push(x as u32)
    // }
    for it in 0..100 {
        print!("\r{}", it);
        // println!("{:?}", n);
        n = move_cups(&n);
    }
    println!();
    println!("{:?}", n);
}
