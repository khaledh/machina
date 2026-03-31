requires {
    std::io::print
    std::io::println
}

// Runtime externs: implemented in runtime/*
@runtime
fn __rt_print(s: string, newline: u64);

@runtime
fn __rt_u64_to_dec(inout buf: u8[], value: u64) -> u64;

@runtime
fn __rt_memset(inout buf: u8[], value: u8);

@runtime
fn __rt_string_from_bytes(out dst: string, bytes: u8[]);

@runtime
fn __rt_args_len() -> u64;

@runtime
fn __rt_arg_at(out dst: string, index: u64);

@runtime
fn __rt_file_last_errno() -> u64;

@runtime
fn __rt_file_open_read(path: string) -> u64;

@runtime
fn __rt_file_open_write(path: string) -> u64;

@runtime
fn __rt_file_open_rw(path: string) -> u64;

@runtime
fn __rt_file_read(fd: u64, inout buf: u8[]) -> u64;

@runtime
fn __rt_file_read_all_text(out dst: string, fd: u64);

@runtime
fn __rt_stdin_read_line(out dst: string) -> u64;

@runtime
fn __rt_file_write(fd: u64, data: u8[]) -> u64;

@runtime
fn __rt_file_close(fd: u64) -> u64;

@intrinsic
fn type_of<T>(value: T) -> string;

@intrinsic
fn view_at<T>(addr: vaddr) -> view<T>;

@intrinsic
fn view_slice_at<T>(addr: vaddr, count: u64) -> view<view<T>[]>;

@intrinsic
fn view_array_at<T>(addr: vaddr, count: u64) -> view<T[]>;

@intrinsic
fn ptr_at<T>(addr: vaddr) -> *T;

// Managed machine runtime bridge helpers.
@runtime
fn __mc_machine_runtime_new() -> u64;

@runtime
fn __mc_machine_runtime_free(runtime: u64);

@runtime
fn __mc_machine_runtime_managed_bootstrap_u64() -> u64;

@runtime
fn __mc_machine_runtime_managed_current_u64() -> u64;

@runtime
fn __mc_machine_runtime_managed_shutdown_u64() -> u64;

@runtime
fn __mc_machine_runtime_spawn_u64(runtime: u64, mailbox_cap: u64) -> u64;

@runtime
fn __mc_machine_runtime_start_u64(runtime: u64, machine_id: u64) -> u64;

@runtime
fn __mc_machine_runtime_send_u64(
  runtime: u64,
  dst: u64,
  kind: u64,
  payload0: u64,
  payload1: u64,
) -> u64;

@runtime
fn __mc_machine_runtime_request_u64(
  runtime: u64,
  src: u64,
  dst: u64,
  kind: u64,
  payload0: u64,
  payload1: u64,
) -> u64;

@runtime
fn __mc_machine_runtime_reply_u64(
  runtime: u64,
  src: u64,
  reply_cap_id: u64,
  kind: u64,
  payload0: u64,
  payload1: u64,
) -> u64;

@runtime
fn __mc_machine_runtime_bind_dispatch_u64(
  runtime: u64,
  machine_id: u64,
  dispatch_fn: u64,
  dispatch_ctx: u64,
) -> u64;

@runtime
fn __mc_machine_runtime_register_thunk_u64(thunk_id: u64, dispatch_fn: u64);

@runtime
fn __mc_machine_runtime_bind_dispatch_thunk_u64(
  runtime: u64,
  machine_id: u64,
  thunk_id: u64,
  dispatch_ctx: u64,
) -> u64;

@runtime
fn __mc_machine_runtime_bind_descriptor_u64(
  runtime: u64,
  machine_id: u64,
  descriptor_id: u64,
  initial_state_tag: u64,
) -> u64;

@runtime
fn __mc_machine_runtime_step_u64(runtime: u64) -> u64;

type KeyNotFound = {}
type IterDone = {}

string :: {
  @intrinsic
  prop len: u64 {
    get;
  }

  @intrinsic
  fn lines(self) -> string[*];

  @intrinsic
  fn split(self, delim: string) -> string[*];

  @intrinsic
  @link_name("string_trim_impl")
  fn trim(self) -> string;

  @intrinsic
  @link_name("string_contains_impl")
  fn contains(self, needle: string) -> bool;

  @runtime
  fn append(inout self, other: string);

  @runtime
  fn append_bytes(inout self, bytes: u8[]);
}
