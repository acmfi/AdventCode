use std::fs::read_to_string;

struct Ship {
    x: i64,
    y: i64,
    facing: i64,
}

#[derive(Debug)]
struct ShipWay {
    x: i64,
    y: i64,
    waypoint: (i64, i64),
}

impl Ship {
    fn rotate(&mut self, rotation: i64) {
        self.facing += rotation;
        self.facing = self.facing.rem_euclid(360);
    }

    fn move_ship(&mut self, command: char, param: i64) {
        match command {
            'N' => self.y += param,
            'S' => self.y -= param,
            'E' => self.x += param,
            'W' => self.x -= param,
            'F' => match self.facing {
                0 => self.move_ship('E', param),
                90 => self.move_ship('S', param),
                180 => self.move_ship('W', param),
                270 => self.move_ship('N', param),
                _ => panic!(
                    "The angle provided for the rotation is invalid: {}",
                    self.facing
                ),
            },
            'L' => self.rotate(-param),
            'R' => self.rotate(param),
            _ => panic!(
                "The parsed command for the ship is invalid: {} was provided",
                command
            ),
        }
    }
}

impl ShipWay {
    fn rotate_waypoint(&mut self, rotation: i64) {
        let prev_x = self.waypoint.0;
        let prev_y = self.waypoint.1;
        self.waypoint.0 = prev_x * (f32::to_radians(rotation as f32).cos().round() as i64)
            - prev_y * (f32::to_radians(rotation as f32).sin().round() as i64);
        self.waypoint.1 = prev_x * (f32::to_radians(rotation as f32).sin().round() as i64)
            + prev_y * (f32::to_radians(rotation as f32).cos().round() as i64);
    }

    fn move_ship_waypoint(&mut self, command: char, param: i64) {
        match command {
            'N' => self.waypoint.1 += param,
            'S' => self.waypoint.1 -= param,
            'E' => self.waypoint.0 += param,
            'W' => self.waypoint.0 -= param,
            'F' => {
                self.x += self.waypoint.0 * param;
                self.y += self.waypoint.1 * param
            }
            'L' => self.rotate_waypoint(param),
            'R' => self.rotate_waypoint(-param),
            _ => panic!(
                "The parsed command for the ship is invalid: {} was provided",
                command
            ),
        }
    }
}

fn manhattan(p: (i64, i64), q: (i64, i64)) -> i64 {
    i64::abs(p.0 - q.0) + i64::abs(p.1 - q.1)
}

fn main() {
    let input = read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = input.lines().collect();
    let mut ship = Ship {
        x: 0,
        y: 0,
        facing: 0,
    };
    for l in lines.iter() {
        ship.move_ship(
            *l.chars().collect::<Vec<_>>().get(0).unwrap(),
            l[1..].parse::<i64>().unwrap(),
        )
    }
    let distance_from_start = manhattan((0, 0), (ship.x, ship.y));
    println!("*** 1st star ***");
    println!("{}", distance_from_start);

    // For this second start, the facing angle parameter of the ship is irrelevant
    let mut second_ship = ShipWay {
        x: 0,
        y: 0,
        waypoint: (10, 1),
    };
    for l in lines.iter() {
        second_ship.move_ship_waypoint(
            *l.chars().collect::<Vec<_>>().get(0).unwrap(),
            l[1..].parse::<i64>().unwrap(),
        );
    }
    let distance_waypoint = manhattan((0, 0), (second_ship.x, second_ship.y));
    println!("*** 2nd star ***");
    println!("{}", distance_waypoint);
}
