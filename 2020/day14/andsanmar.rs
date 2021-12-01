use std::fs;
use std::collections::HashMap;

fn update_value(val: usize, mask : &Vec<(usize,Option<bool>)>) -> usize {
    mask.iter().fold(val, |x, (pos,v)| match v {
            None => x,
            Some(true) => x | 1 << (35 - pos),
            Some(false) => x & !(1 << (35 - pos))
    })
}

fn update_address(val: usize, mask : &Vec<(usize,Option<bool>)>) -> Vec<usize> {
    mask.iter().fold(vec!(val), |s, (pos,v)| match v {
        None => {
            let mut new_s : Vec<usize> = s.iter().map(|x| x | 1 << (35 - pos)).collect(); // Set it at 1
            new_s.append(&mut s.iter().map(|x| x & !(1 << (35 - pos))).collect()); // Set it at 0
            new_s
        },
        Some(true) => s.iter().map(|x| x | 1 << (35 - pos)).collect(),
        Some(false) => s
    })
}

fn main () {
    let r = fs::read_to_string("input").unwrap();
    let mut current_mask : Vec<(usize, Option<bool>)> = Vec::new(); // index,val
    let mut mem1 : HashMap<usize, usize> = HashMap::new();
    for l in r.lines() {
        let mut it = l.split('=');
        let var = it.next().unwrap().trim();
        let value = it.next().unwrap().trim();
        match var {
            "mask" => current_mask = value.chars().enumerate()//.filter(|(_,x)| *x != 'X')
                .map(|(i,v)| (i, if v == 'X' {None} else {Some(v=='1')})).collect(),
            _ => {
                let pos : usize = var[4..(var.len()-1)].parse().unwrap();
                let to_insert = update_value(value.parse().unwrap(), &current_mask);
                mem1.insert(pos, to_insert);
            }
        }
    }
    println!("{}", mem1.values().sum::<usize>());
    let mut mem2 : HashMap<usize, usize> = HashMap::new();
    for l in r.lines() {
        let mut it = l.split('=');
        let var = it.next().unwrap().trim();
        let value = it.next().unwrap().trim();
        match var {
            "mask" => current_mask = value.chars().enumerate()//.filter(|(_,x)| *x != 'X')
                .map(|(i,v)| (i, if v == 'X' {None} else {Some(v=='1')})).collect(),
            _ => {
                let pos : usize = var[4..(var.len()-1)].parse().unwrap();
                for pos in update_address(pos, &current_mask) {
                    mem2.insert(pos, value.parse().unwrap());
                }
            }
        }
    }
    println!("{}", mem2.values().sum::<usize>())
}
