requires {
    std::io::EndOfInput
    std::io::IoError
    std::io::print
    std::io::println
    std::io::stdin
    std::parse::ParseError
    std::parse::parse_u64
}

type Board = {
    cells: u64[9],
}

fn player_mark(player: u64) -> string {
    player == 1 ? "X" : "O"
}

fn cell_label(board: Board, index: u64) -> string {
    match board.cells[index] {
        1 => "X",
        2 => "O",
        _ => f"{index + 1}",
    }
}

fn print_board(board: Board) {
    println("");
    println(f" {cell_label(board, 0)} | {cell_label(board, 1)} | {cell_label(board, 2)} ");
    println("---+---+---");
    println(f" {cell_label(board, 3)} | {cell_label(board, 4)} | {cell_label(board, 5)} ");
    println("---+---+---");
    println(f" {cell_label(board, 6)} | {cell_label(board, 7)} | {cell_label(board, 8)} ");
    println("");
}

fn line_is_winner(board: Board, a: u64, b: u64, c: u64, player: u64) -> bool {
    board.cells[a] == player &&
    board.cells[b] == player &&
    board.cells[c] == player
}

fn has_winner(board: Board, player: u64) -> bool {
    // Horizontal wins
    line_is_winner(board, 0, 1, 2, player) ||
    line_is_winner(board, 3, 4, 5, player) ||
    line_is_winner(board, 6, 7, 8, player) ||
    // Vertical wins
    line_is_winner(board, 0, 3, 6, player) ||
    line_is_winner(board, 1, 4, 7, player) ||
    line_is_winner(board, 2, 5, 8, player) ||
    // Diagonal wins
    line_is_winner(board, 0, 4, 8, player) ||
    line_is_winner(board, 2, 4, 6, player)
}

fn is_draw(board: Board) -> bool {
    for cell in board.cells {
        if cell == 0 {
            return false;
        }
    }
    true
}

fn apply_move(inout board: Board, square: u64, player: u64) -> bool {
    if square < 1 || square > 9 {
        return false;
    }

    let index = square - 1;
    if board.cells[index] != 0 {
        return false;
    }

    board.cells[index] = player;
    true
}

fn read_move(player: u64) -> u64 | IoError | EndOfInput | ParseError {
    print(f"Player {player_mark(player)}, choose a square (1-9): ");
    let line = stdin().read_line()?;
    parse_u64(line.trim())
}

fn main() -> () | IoError {
    println("Machina Tic-Tac-Toe");
    println("Enter a square number from 1 to 9.");

    var board = Board {
        cells: [0; 9],
    };
    var player = 1;

    while true {
        print_board(board);

        match read_move(player) {
            square: u64 => {
                if !apply_move(inout board, square, player) {
                    println("That square is not available.");
                    continue;
                }
            }
            done: EndOfInput => {
                println("Goodbye.");
                return ();
            }
            err: IoError => {
                return err;
            }
            bad: ParseError => {
                println("Please enter a number from 1 to 9.");
                continue;
            }
        }

        if has_winner(board, player) {
            print_board(board);
            println(f"Player {player_mark(player)} wins!");
            return ();
        }

        if is_draw(board) {
            print_board(board);
            println("It's a draw.");
            return ();
        }

        player = player == 1 ? 2 : 1;
    }
}
