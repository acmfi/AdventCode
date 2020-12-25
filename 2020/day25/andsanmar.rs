use std::fs;

fn retrieve_loop_size(pk : usize) -> usize {
    let subject_number = 7;
    let mut value : usize = 1;
    let mut loop_size = 0;
    while value != pk {
        value = (value*subject_number) % 20201227;
        loop_size += 1;
    }
    loop_size
}

fn main() {
    let n: Vec<usize> = fs::read_to_string("input").unwrap().lines().map(|x| x.parse::<usize>().unwrap()).collect();
    let door_pk = n[0];
    let card_pk = n[1];
    let door_sk = retrieve_loop_size(door_pk);
    // let card_sk = retrieve_loop_size(card_pk);
    let encryption_key = (0..door_sk).fold(1,|value,_| (value*card_pk) % 20201227);
    // let encryption_key = (0..card_sk).fold(1,|value,_| (value*door_pk) % 20201227);
    println!("{}", encryption_key);
}
