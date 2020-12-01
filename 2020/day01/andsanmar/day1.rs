use std::fs;

fn star1(nums : &Vec<u32>) {
    for x in nums.iter() {
        for y in nums.iter() {
            if x + y  == 2020 {
                println!("{}", x * y);
                return
            }
        }
    }
}

fn star2(nums : &Vec<u32>) {
    for x in nums.iter() {
        for y in nums.iter() {
            for z in nums.iter() {
                if x + y + z == 2020 {
                    println!("{}", x * y * z);
                    return
                }
            }
        }
    }
}

fn main() {
    let nums : Vec<_> = fs::read_to_string("input").unwrap().split_whitespace().map(|x| x.parse::<u32>().unwrap()).collect();

    star1(&nums);
    star2(&nums);
}
