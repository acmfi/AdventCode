use std::fs;
use std::collections::HashMap;

fn main () {
    let r = fs::read_to_string("input").unwrap();
    let nums : Vec<usize> = r.split(',').map(|x| x.parse().unwrap()).collect();
    let mut last_time_spoken : HashMap<usize, usize> = HashMap::new();
    for (turn,num) in nums[..nums.len()-1].iter().enumerate() {
        last_time_spoken.insert(*num,turn);
    }
    let mut last_spoken_number : usize = *nums.last().unwrap();
    for turn in nums.len()..30000000 {
        if turn == 2020 {println!("{}", last_spoken_number)}
        let new_num = match last_time_spoken.get(&last_spoken_number) {
            None => {
                0
            },
            Some(last_time) => {
                (turn-1)-last_time
            }
        };
        last_time_spoken.insert(last_spoken_number, turn-1);
        last_spoken_number = new_num;
    }
    println!("{}", last_spoken_number);
}
