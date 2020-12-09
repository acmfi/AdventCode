use std::fs;

fn is_sum_of(r : usize, prev : &[usize]) -> bool {
    prev.iter().filter(|x| prev.contains(&(r-*x)) && r != *x*2 ).count() > 0
}

fn find_summing(r : usize, nums : &Vec<usize>) {
    for x in 0..nums.len() {
        let mut adding = 0;
        for y in x.. {
            adding += nums[y];
            if adding > r || r == nums[y] {
                break
            } else if adding == r {
                let range = nums[x..y+1].to_vec();
                let min = range.iter().min().unwrap();
                let max = range.iter().max().unwrap();
                println!("{} {} {}", min, max, min+max);
            }
        }
    }
}

fn main () {
    let r = fs::read_to_string("input").unwrap();
    let nums : Vec<usize> = r.lines().map(|l| l.parse::<usize>().unwrap()).collect();
    for x in 25..nums.len() {
        if ! is_sum_of(nums[x], &nums[x-25..x]) {
            println!("{:?}", nums[x]);
            find_summing(nums[x], &nums);
            break
        }
    }
    
}
