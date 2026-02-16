type AppError
  = NotFound
  | InvalidCode(u64)

fn ok(value: u64) -> u64 | AppError {
    value
}

fn fail() -> u64 | AppError {
    AppError::InvalidCode(404)
}

fn maybe_read(success: bool) -> u64 | AppError {
    if success {
        ok(0)
    } else {
        fail()
    }
}

// Demonstrates executable entrypoint handling for `main` returning an error union.
//
// Default behavior (success = true):
// - program succeeds and exits with code 0.
//
// To observe unhandled error reporting at the entrypoint, set:
//   success = false
// Then the generated entry wrapper reports:
//   Runtime error: Unhandled error in main: AppError::InvalidCode
// and exits non-zero.
fn main() -> u64 | AppError {
    let success = true;
    maybe_read(success)?
}
