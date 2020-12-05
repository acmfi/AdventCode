use std::cmp;
//use std::collections::HashSet;
use std::fs::read_to_string;

fn get_seat_info(raw_pass: &str) -> (u16, u16, u16) {
    let ((row, _), (col, _)) = raw_pass.chars()
	.fold(((0, 127), (0, 7)),
	      |((x1, x2), (y1, y2)), c| approximate(((x1, x2), (y1, y2)), c).unwrap());
    return (row, col, row * 8 + col);
}

fn approximate(min_max_row_cols: ((u16, u16), (u16, u16)),
		  c: char) -> Option<((u16, u16), (u16, u16))> {
    let ((mut min_row, mut max_row), (mut min_col, mut max_col)) = min_max_row_cols;
    let difference_row = (max_row - min_row + 1)/2;
    let difference_col = (max_col - min_col + 1)/2;
    if c == 'F' {
	match difference_row {
	    1 => max_row -= 1,
	    _ => max_row -= difference_row,
	}
    } else if c == 'B' {
	match difference_row {
	    1 => min_row += 1,
	    _ => min_row += difference_row,
	}
    } else if c == 'L' {
	match difference_col {
	    1 => max_col -= 1,
	    _ => max_col -= difference_col,	    
	}
    } else if c == 'R' {
	match difference_col {
	    1 => min_col += 1,
	    _ => min_col += difference_col,	    
	}
    } else { return None }
    return Some(((min_row, max_row), (min_col, max_col)));
}

fn main() -> std::io::Result<()> {
    let f = read_to_string("input.txt")?;
    let passes: Vec<(u16, u16, u16)> = f.lines().map(|line| get_seat_info(line)).collect();
    let max_id = passes.iter().fold(0, |prev_max, (_, _, id)| cmp::max(prev_max, *id));
    let sets_ids: Vec<&u16> = passes.iter()
	.map(|(_,_,id1)| passes.iter()
	     .filter(|&(_,_,id2)| id1 == id2
		     || *id1 == id2 - 1
		     || *id1 == id2 + 1)
	     .collect::<Vec<&(u16, u16, u16)>>())
	.filter(|sublist| sublist.len() == 2)
	.map(|sublist| sublist.iter()
	     .map(|(_,_,id)| id).collect::<Vec<&u16>>())
	.flatten().collect();
    println!("*** 1st star ***");
    println!("{}", max_id);
    println!("*** 2nd star ***");
    // My 2nd star is completely stupid, overly complex
    // and hard to understand; sorry about that :(
    println!("{}", sets_ids[7]-1);
    Ok(())
}
// This is here because I tried to make the solution to the 2nd star
// to spit out the actual number, but I REALLY need to learn how
// borrowing and ownership work to get further...

// .map(|id| { let mut set = HashSet::new();
// 	    set.insert(id - 1);
// 	    set.insert(id + 1);
// 	    return set; })
// .collect();
