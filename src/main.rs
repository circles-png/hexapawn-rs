#![warn(clippy::nursery, clippy::pedantic)]
#![feature(iter_next_chunk)]
use rand::prelude::SliceRandom;
use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
    hash::{DefaultHasher, Hash, Hasher},
    ops::{Deref, Index, IndexMut, Neg, Not},
};

use strum::VariantArray;

const BOARD_SIZE: usize = 3;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Position(usize, usize);

impl Position {
    const fn x(&self) -> usize {
        self.0
    }
    #[allow(clippy::cast_possible_truncation)]
    const fn file(&self) -> char {
        (b'a' + self.x() as u8) as char
    }
    const fn y(&self) -> usize {
        self.1
    }
    const fn rank(&self) -> usize {
        BOARD_SIZE - self.y()
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.file(), self.rank())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Delta {
    Neg(usize),
    Pos(usize),
}

impl Neg for Delta {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Neg(delta) => Self::Pos(delta),
            Self::Pos(delta) => Self::Neg(delta),
        }
    }
}

impl Delta {
    const fn apply(self, x: usize) -> Option<usize> {
        match self {
            Self::Neg(delta) => x.checked_sub(delta),
            Self::Pos(delta) => x.checked_add(delta),
        }
    }
}

#[derive(Debug, VariantArray, Clone, Copy, PartialEq, Eq, Hash)]
enum Colour {
    White,
    Black,
}

impl Display for Colour {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::White => "♟",
                Self::Black => "♙",
            }
        )
    }
}

impl Colour {
    const fn forward_delta(self) -> Delta {
        match self {
            Self::White => Delta::Neg(1),
            Self::Black => Delta::Pos(1),
        }
    }
    const fn left_delta(self) -> Delta {
        self.forward_delta()
    }

    fn right_delta(self) -> Delta {
        -self.left_delta()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Piece {
    position: Position,
    colour: Colour,
}

impl Piece {
    fn forward(&self) -> Option<Position> {
        let Position(x, y) = self.position;
        let forward = Position(x, self.colour.forward_delta().apply(y)?);
        if forward.y() < BOARD_SIZE {
            return Some(forward);
        }
        None
    }
    fn attack_left(&self) -> Option<Position> {
        let Position(x, y) = self.forward()?;
        let position = Position(self.colour.left_delta().apply(x)?, y);
        if position.x() < BOARD_SIZE {
            return Some(position);
        }
        None
    }
    fn attack_right(&self) -> Option<Position> {
        let Position(x, y) = self.forward()?;
        let position = Position(self.colour.right_delta().apply(x)?, y);
        if position.x() < BOARD_SIZE {
            return Some(position);
        }
        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct State {
    pieces: HashSet<Piece>,
}

impl Hash for State {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

#[derive(Debug, Clone)]
struct Player {
    colour: Colour,
    strategy: HashMap<State, HashMap<Action, usize>>,
    history: HashMap<State, Action>,
}

impl Player {
    fn new(colour: Colour) -> Self {
        Self {
            colour,
            strategy: HashMap::new(),
            history: HashMap::new(),
        }
    }
    #[allow(clippy::cast_precision_loss)]
    fn play(&mut self, state: &State) -> Result<State, ()> {
        let action_space = state.action_space(self.colour);
        let total = action_space
            .iter()
            .map(|action| {
                *self
                    .strategy
                    .entry(state.clone())
                    .or_default()
                    .entry(*action)
                    .or_insert(2) as f32
            })
            .sum::<f32>();
        for action in &action_space {
            println!(
                "   {action}: {:.2}%",
                *self.strategy.get(state).unwrap().get(action).unwrap() as f32 / total * 100.
            );
        }
        for action in &action_space {
            self.strategy
                .entry(state.clone())
                .or_default()
                .entry(*action)
                .or_insert(2);
        }
        let chosen = action_space
            .choose_weighted(&mut rand::thread_rng(), |action| {
                *self.strategy.get(state).unwrap().get(action).unwrap()
            })
            .map_err(|_| ())?;
        println!(
            "   chosen: {chosen} {:.2}%",
            *self.strategy.get(state).unwrap().get(chosen).unwrap() as f32 / total * 100.
        );
        self.history.insert(state.clone(), *chosen);
        let mut next = state.clone();
        next.apply(chosen).unwrap();
        Ok(next)
    }

    fn reward(&mut self, value: usize) {
        for (state, action) in &self.history {
            *self
                .strategy
                .get_mut(state)
                .unwrap()
                .get_mut(action)
                .unwrap() += value;
        }
        self.history.clear();
    }

    fn punish(&mut self, value: usize) {
        for (state, action) in &self.history {
            *self
                .strategy
                .get_mut(state)
                .unwrap()
                .get_mut(action)
                .unwrap() -= value;
        }
        self.history.clear();
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Action {
    from: Position,
    to: Position,
    captures: bool,
}

impl PartialOrd for Action {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Action {
    fn cmp(&self, other: &Self) -> Ordering {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        let self_hash = hasher.finish();
        let mut hasher = DefaultHasher::new();
        other.hash(&mut hasher);
        let other_hash = hasher.finish();
        self_hash.cmp(&other_hash)
    }
}

impl Display for Action {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{} ({} -> {})",
            self.from.file(),
            if self.captures { "x" } else { "" },
            if self.captures {
                self.to.to_string()
            } else {
                self.to.rank().to_string()
            },
            self.from,
            self.to
        )
    }
}

impl State {
    fn new() -> Self {
        Self {
            pieces: {
                Colour::VARIANTS
                    .iter()
                    .zip([BOARD_SIZE - 1, 0])
                    .flat_map(|(colour, y)| {
                        (0..BOARD_SIZE).map(move |x| Piece {
                            position: Position(x, y),
                            colour: *colour,
                        })
                    })
                    .collect()
            },
        }
    }

    fn occupied(&self, x: usize, y: usize) -> Option<Colour> {
        self.pieces
            .iter()
            .find(|piece| piece.position == Position(x, y))
            .map(|piece| piece.colour)
    }

    const fn inbounds(x: usize, y: usize) -> bool {
        x < BOARD_SIZE && y < BOARD_SIZE
    }

    fn action_space(&self, colour: Colour) -> Vec<Action> {
        self.pieces
            .iter()
            .filter(|piece| piece.colour == colour)
            .flat_map(|piece| {
                let mut actions = Vec::new();
                if let Some(Position(x, y)) = piece.forward() {
                    if Self::inbounds(x, y) && self.occupied(x, y).is_none() {
                        actions.push(Action {
                            from: piece.position,
                            to: Position(x, y),
                            captures: false,
                        });
                    }
                }
                for &attack in &[piece.attack_left(), piece.attack_right()] {
                    if let Some(Position(x, y)) = attack {
                        if Self::inbounds(x, y)
                            && self
                                .occupied(x, y)
                                .map_or(false, |colour| colour != piece.colour)
                        {
                            actions.push(Action {
                                from: piece.position,
                                to: Position(x, y),
                                captures: true,
                            });
                        }
                    }
                }
                actions.sort_unstable();
                actions.dedup();
                actions
            })
            .collect()
    }

    fn apply(&mut self, action: &Action) -> Result<(), ()> {
        let colour = self
            .pieces
            .iter()
            .find(|piece| piece.position == action.from)
            .ok_or(())?
            .colour;
        self.pieces = self
            .pieces
            .drain()
            .map(|mut piece| {
                if piece.position == action.from {
                    piece.position = action.to;
                }
                piece
            })
            .collect();
        #[allow(clippy::suspicious_operation_groupings)]
        if action.captures {
            self.pieces
                .retain(|other| !(other.position == action.to && other.colour != colour));
        }
        Ok(())
    }

    fn winner(&self) -> Option<Colour> {
        Colour::VARIANTS
            .iter()
            .find(|&&colour| {
                self.pieces
                    .iter()
                    .filter(|piece| piece.colour != colour)
                    .count()
                    == 0
                    || self
                        .pieces
                        .iter()
                        .filter(|piece| piece.colour == colour)
                        .any(|piece| piece.forward().is_none())
            })
            .copied()
    }
}

impl Display for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for y in 0..BOARD_SIZE {
            for x in 0..BOARD_SIZE {
                if let Some(colour) = self.occupied(x, y) {
                    write!(f, "{colour}")?;
                } else {
                    write!(f, ".")?;
                }
            }
            if y < BOARD_SIZE - 1 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
struct CurrentPlayer(usize);

impl CurrentPlayer {
    const fn new() -> Self {
        Self(0)
    }

    fn flip(&mut self) {
        *self = -*self;
    }
}

impl Neg for CurrentPlayer {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(1 - self.0)
    }
}

impl Not for CurrentPlayer {
    type Output = Self;

    fn not(self) -> Self::Output {
        -self
    }
}

impl Deref for CurrentPlayer {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Index<CurrentPlayer> for Vec<Player> {
    type Output = Player;

    fn index(&self, index: CurrentPlayer) -> &Self::Output {
        &self[*index]
    }
}

impl IndexMut<CurrentPlayer> for Vec<Player> {
    fn index_mut(&mut self, index: CurrentPlayer) -> &mut Self::Output {
        &mut self[*index]
    }
}

#[derive(Debug)]
struct Game<'a> {
    state: State,
    players: &'a mut Vec<Player>,
    current_player: CurrentPlayer,
}

impl<'a> Game<'a> {
    fn new(players: &'a mut Vec<Player>) -> Self {
        Self {
            state: State::new(),
            players,
            current_player: CurrentPlayer::new(),
        }
    }

    fn current_player_mut(&mut self) -> &mut Player {
        &mut self.players[self.current_player]
    }

    fn run(&mut self) {
        enum Outcome {
            Win(Colour),
            Draw,
        }
        let outcome = loop {
            println!("{}", self.state);
            if let Some(winner) = self.state.winner() {
                break Outcome::Win(winner);
            }
            let state = self.state.clone();
            if let Ok(next) = self.current_player_mut().play(&state) {
                self.state = next;
                self.current_player.flip();
            } else {
                break Outcome::Draw;
            }
        };
        match outcome {
            Outcome::Win(winner) => {
                println!("{winner} wins");
                let loser = self
                    .players
                    .iter_mut()
                    .find(|player| player.colour != winner)
                    .unwrap();
                loser.punish(1);
                let winner = self
                    .players
                    .iter_mut()
                    .find(|player| player.colour == winner)
                    .unwrap();
                winner.reward(3);
            }
            Outcome::Draw => {
                println!("draw");
                for player in &mut *self.players {
                    player.reward(1);
                }
            }
        }
    }
}

fn main() {
    let mut players = Colour::VARIANTS
        .iter()
        .map(|&colour| Player::new(colour))
        .collect();
    loop {
        Game::new(&mut players).run();
    }
}
