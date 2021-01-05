use std::fs;

fn first_max(min: usize, current: usize, step: &usize) -> usize {
    if current > min { current } else { first_max(min, current+step, step)}
}

fn get_min_timestamp(ids  : Vec<(usize, usize)>) -> usize {
    let mut timestamp = 0;
    let mut inc = 1;
    // CRT
    for (i,id) in ids {
        while (timestamp + i) % id != 0 {
            timestamp += inc;
        }
        inc *= id;
    }
    timestamp
}

fn main () {
    let r = fs::read_to_string("input").unwrap();
    let mut it = r.lines();
    let earliest_timestamp : usize = it.next().unwrap().parse::<usize>().unwrap();
    let bus_ids : Vec<(usize,usize)> = it.next().unwrap().split(',').enumerate().filter(|s| s.1 != "x").map(|(i,n)| (i,n.parse::<usize>().unwrap())).collect();
    println!("{:?}", bus_ids);
    let differences = bus_ids.iter().map(|(_,n)| (first_max(earliest_timestamp, 0, n) - earliest_timestamp,n));
    println!("{:?}", {let min = differences.min().unwrap(); min.0*min.1});
// let min_ts = get_min_timestamp(1.., &mut bus_ids.iter());
    let min_ts = get_min_timestamp(bus_ids);
    println!("{:?}", min_ts);
}
