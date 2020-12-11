use std::fs;

fn is_sum_of(r : usize, prev : &[usize]) -> bool {
    prev.iter().filter(|x| prev.contains(&(r-*x)) && r != *x*2 ).count() > 0
}

fn find_summing(r : usize, nums : &Vec<usize>) -> usize {
    let mut adding = 0;
    let mut min = 0;
    for max in 0..nums.len() {
        adding += nums[max];
        while adding > r {
            adding -= nums[min];
            min += 1;
        }
        if adding == r {
            let v : Vec<usize> = nums[min..max+1].to_vec();
            let min = v.iter().min().unwrap();
            let max = v.iter().max().unwrap();
            return min+max;
        }
    }
    0
}


fn main () {
    let r = fs::read_to_string("input").unwrap();
    let nums : Vec<usize> = r.lines().map(|l| l.parse::<usize>().unwrap()).collect();
    for x in 25..nums.len() {
        if ! is_sum_of(nums[x], &nums[x-25..x]) {
            println!("{:?}", nums[x]);
            println!("{}", find_summing(nums[x], &nums));
            break
        }
    }    
}
