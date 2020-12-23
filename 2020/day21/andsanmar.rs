use std::fs;
use std::collections::HashMap;

fn intersect_all<'a>(l : &Vec<&Vec<&'a str>>) -> Vec<&'a str> {
    let (first, rest) = l.split_first().unwrap();
    // TODO instead of `clone` use reference
    let mut intersection : Vec<&str> = first.clone().clone();
    for x in rest {
        intersection = intersection.iter().filter(|e| x.contains(e)).map(|e| *e).collect();
    }
    intersection
}

fn main () {
    let r = fs::read_to_string("input").unwrap();
    let foods : Vec<(Vec<&str>,Vec<&str>)> = r.lines().map(|l| {
        let mut it = l.split("(contains");
        let ingredients : Vec<&str> = it.next().unwrap().split_whitespace().collect();
        let allergens = it.next().unwrap();
        (ingredients, allergens[0..allergens.len()-1].split(',').map(|al| al.trim()).collect())
    }).collect();
    let mut m : HashMap<&str,Vec<&Vec<&str>>> = HashMap::new();
    for (ingredients,l_allergens) in &foods {
        for allergen in l_allergens {
            if m.get(allergen) == None { m.insert(allergen,Vec::new()); }
            m.get_mut(allergen).unwrap().push(ingredients);
        }
    }
    let mut allergens : Vec<&str> = m.values().map(|l| intersect_all(l)).flatten().collect();
    allergens.sort();
    allergens.dedup();
    let not_listed : usize = foods.iter().map(|(ingredients,_)| ingredients.iter().filter(|i| !allergens.contains(i)).count()).sum();
    println!("{}", not_listed);
    let mut allergens : Vec<(&str,Vec<&str>)> = m.iter().map(|(a,l)| (*a,intersect_all(l))).collect();
    // We sort it to ensure all the elements with a single match are added
    allergens.sort_by(|(_i1,c1),(_i2,c2)| c1.len().partial_cmp(&c2.len()).unwrap()); // Sort by length
    let mut allergens_unique : Vec<(&str,&str)> = Vec::new();
    while allergens_unique.len() != allergens.len() {
        for (a,l) in &allergens {
            match l.iter().find(|i| allergens_unique.iter().filter(|(_,j)| *i==j).count() == 0) {
                None => {},
                Some(i) => allergens_unique.push((a,i))
            }
        }
    }
    allergens_unique.sort();
    println!("{:?}", allergens_unique);//.iter().map(|(_,i)| *i).collect::<Vec<&str>>().join(","));
}
