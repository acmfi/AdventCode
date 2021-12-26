fn increment_dice (dice : &mut u64, dice_limit : u64) {
    *dice += 1;
    if *dice > dice_limit { *dice -= dice_limit }
}
    
fn star1 (mut p1 : u64, mut p2 : u64, limit_score : u64, dice_limit : u64) {
    let mut dice : u64 = 1;
    let mut dice_rolled = 0;
    let mut p1_score : u64 = 0;
    let mut p2_score : u64 = 0;
    let mut turn_p1 = true;
    while p1_score < limit_score && p2_score < limit_score {
        if turn_p1 {
            for _ in 0..3 {
                p1 += dice;
                increment_dice(&mut dice, dice_limit);
            }
            p1 %= 10;
            if p1 == 0 { p1 = 10; }
            p1_score += p1;
        } else {
            for _ in 0..3 {
                p2 += dice;
                increment_dice(&mut dice, dice_limit);
            }
            p2 %= 10;
            if p2 == 0 { p2 = 10; }
            p2_score += p2;
        }
        dice_rolled += 3;
        turn_p1 = !turn_p1;
    }
    let min_punctuation = p1_score.min(p2_score);
    println!("{}", min_punctuation * dice_rolled)
}

fn star2_aux (p1 : u8, p2 : u8,
              p1_score : u8, p2_score : u8,
              won_p1 : &mut usize, won_p2 : &mut usize,
              turn_p1 : bool, limit_score : u8,
              cumulative_scores : &[(u8, usize);7], cases : usize) {
    if p1_score >= limit_score {
        *won_p1 += cases;
        return
    } else if p2_score >= limit_score {
        *won_p2 += cases;
        return
    }
    for (s, times) in cumulative_scores {
        if turn_p1 {
            let mut new_p1 = p1;
            new_p1 += s;
            new_p1 %= 10;
            if new_p1 == 0 { new_p1 = 10 }
            let new_p1_score = p1_score + new_p1;
            star2_aux(new_p1,p2,
                      new_p1_score, p2_score,
                      won_p1, won_p2,
                      !turn_p1, limit_score,
                      cumulative_scores, cases*times)
        } else {
            let mut new_p2 = p2;
            new_p2 += s;
            new_p2 %= 10;
            if new_p2 == 0 { new_p2 = 10 }
            let new_p2_score = p2_score + new_p2;
            star2_aux(p1,new_p2,
                      p1_score, new_p2_score,
                      won_p1, won_p2,
                      !turn_p1, limit_score,
                      cumulative_scores, cases*times)
        }
    }
}

fn star2 (p1 : u64, p2 : u64) {
    let mut possible_scores : Vec<u8> = vec![];
    for i in 1..=3 {
        for j in 1..=3 {
            for k in 1..=3 {
                possible_scores.push(i+j+k);
            }
        }
    }
    let cumulative_scores : [(u8, usize);7] =
        [(3, possible_scores.iter().filter(|a| **a == 3).count()),
         (4, possible_scores.iter().filter(|a| **a == 4).count()),
         (5, possible_scores.iter().filter(|a| **a == 5).count()),
         (6, possible_scores.iter().filter(|a| **a == 6).count()),
         (7, possible_scores.iter().filter(|a| **a == 7).count()),
         (8, possible_scores.iter().filter(|a| **a == 8).count()),
         (9, possible_scores.iter().filter(|a| **a == 9).count())];
    let mut won_p1 = 0;
    let mut won_p2 = 0;
    star2_aux(p1 as u8, p2 as u8,
              0, 0,
              &mut won_p1, &mut won_p2,
              true, 21,
              &cumulative_scores, 1);
    println!("{}", won_p1.max(won_p2));
}

fn parse_input<'a>(s : &'a String) -> (u64, u64) {
    let mut i = s.lines();
    (i.next().unwrap().split_whitespace().last().unwrap().parse().unwrap(),
     i.next().unwrap().split_whitespace().last().unwrap().parse().unwrap())
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let filename = std::fs::read_to_string(args[1].clone()).unwrap();
    let (p1, p2): (u64, u64) = parse_input(&filename);
    star1(p1, p2, 1000, 100);
    star2(p1,p2);
}
