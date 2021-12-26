use std::collections::HashSet;
type Image = HashSet<Option<(isize,isize)>>;

fn print_image(image : &Image) {
    let min_x = image.iter().filter_map(|e| *e).min_by(|(a,_),(b,_)| a.cmp(b)).unwrap().0;
    let max_x = image.iter().filter_map(|e| *e).max_by(|(a,_),(b,_)| a.cmp(b)).unwrap().0;
    let min_y = image.iter().filter_map(|e| *e).min_by(|(_,a),(_,b)| a.cmp(b)).unwrap().1;
    let max_y = image.iter().filter_map(|e| *e).max_by(|(_,a),(_,b)| a.cmp(b)).unwrap().1;
    for x in min_x..=max_x {
        for y in min_y..=max_y {
            print!("{}",
                   if image.contains(&Some((x,y))) {
                       "#"
                   } else {
                       "."
                   }
            )
        }
        println!("");
    }
}

// from day16
fn bin_to_hex(acc : usize, n : &[bool]) -> usize {
    match n {
        [] => acc,
        [false,..] => bin_to_hex(acc << 1, &n[1..]),
        [true,..] => bin_to_hex((acc << 1) | 1 , &n[1..])
    }
}

fn stars(alg: &Vec<bool>, mut image: Image, n : usize) {
    for _ in 0..n {
        let mut new_image : Image = HashSet::new();
        match image.contains(&None) {
            true => if *alg.last().unwrap() {
                new_image.insert(None);
            },
            false => if alg[0] {
                new_image.insert(None);
            }
        }
        let min_x = image.iter().filter_map(|e| *e).min_by(|(a,_),(b,_)| a.cmp(b)).unwrap().0;
        let max_x = image.iter().filter_map(|e| *e).max_by(|(a,_),(b,_)| a.cmp(b)).unwrap().0;
        let min_y = image.iter().filter_map(|e| *e).min_by(|(_,a),(_,b)| a.cmp(b)).unwrap().1;
        let max_y = image.iter().filter_map(|e| *e).max_by(|(_,a),(_,b)| a.cmp(b)).unwrap().1;
        for x in min_x-1..=max_x+1 {
            for y in min_x-1..=max_x+1 {
                let index = [(x-1,y-1),(x-1,y),(x-1,y+1),
                             (x,  y-1),(x,  y),(x,  y+1),
                             (x+1,y-1),(x+1,y),(x+1,y+1)]
                    .map(|(i,j)| {
                        let to_check =
                            if i >= min_x && i <= max_x &&
                            j >= min_y && j <= max_y {
                                Some((i,j))
                            } else {
                                None
                            };
                        image.contains(&to_check)
                    });
                let new_pixel = alg[bin_to_hex(0,&index)];
                if new_pixel {
                    new_image.insert(Some((x,y)));
                }       
            }
        }
        image = new_image;
    }
    println!("{:?}", image.len());
}

fn parse_input<'a>(s : &'a String) -> (Vec<bool>, Image) {
    let mut i = s.split("\n\n");
    let algorithm = i.next().unwrap().chars().map(|c| match c {
        '#' => true,
        _ => false
    }).collect();
    let image = i.next().unwrap().lines().enumerate().map(|(i,l)| l.chars().enumerate().filter(|(_,c)| match c {
        '#' => true,
        _ => false
    }).map(|(j,_)| {
        Some((i as isize,j as isize))
    }).collect::<HashSet<Option<(isize,isize)>>>()).flatten().collect();
    (algorithm,image)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Feed me with the input!");
        std::process::exit(1);
    };
    let filename = std::fs::read_to_string(args[1].clone()).unwrap();
    let (alg, image): (Vec<bool>, Image) = parse_input(&filename);
    stars(&alg, image.clone(), 2);
    stars(&alg, image, 50);
}
