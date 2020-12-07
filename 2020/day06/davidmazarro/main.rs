use std::fs::read_to_string;

fn main() -> std::io::Result<()> {
    let input = read_to_string("input.txt")?;
    let lines: Vec<&str> = input.lines().collect();
    let groups: Vec<&[&str]> = lines.split(|&s| s == "").collect();

    let counts: Vec<usize> = groups
        .iter()
        .map(|group| {
            let unsorted: &mut Vec<char> = &mut group.concat().chars().collect();
            unsorted.sort();
            unsorted.dedup();
            let count = unsorted.len();
            count
        })
        .collect();
    let sum_counts: usize = counts.iter().sum();

    let counts_everyone: Vec<usize> = groups
	.iter().map(|group| {
	    let mut group_copy = group.clone().to_vec();
	    let group_ref = &mut group_copy;
	    group_ref.sort();
	    group_ref.sort_unstable_by(|x, y| x.len().cmp(&y.len()));
	    group.iter().map(|list| list.chars()
			     .filter(|&c| group[0].contains(c))
			     .collect::<Vec<char>>())
		.map(|l| l.len()).min()
	})
	.map(|l| l.unwrap())
	.collect();
    let sum_counts_everyone: usize = counts_everyone.iter().sum();

    println!("*** 1st star ***");
    println!("{}", sum_counts);
    println!("*** 2nd star ***");
    println!("{}", sum_counts_everyone);
    Ok(())
}
