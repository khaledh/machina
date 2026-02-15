// Managed machine runtime wrappers.
//
// This module intentionally exposes a small, explicit V1 surface while the
// full descriptor/bootstrap path is still evolving.

@[public]
type Runtime = {
  _raw: u64,
}

@[public]
fn new_runtime() -> Runtime {
  Runtime {
    _raw: __mc_machine_runtime_new(),
  }
}

@[public]
fn close_runtime(inout rt: Runtime) {
  __mc_machine_runtime_free(rt._raw);
  rt._raw = 0;
}

@[public]
fn spawn(rt: Runtime, mailbox_cap: u64) -> u64 {
  __mc_machine_runtime_spawn_u64(rt._raw, mailbox_cap)
}

@[public]
fn start(rt: Runtime, machine_id: u64) -> bool {
  __mc_machine_runtime_start_u64(rt._raw, machine_id) != 0
}

@[public]
fn send(rt: Runtime, dst: u64, kind: u64, payload0: u64, payload1: u64) -> u64 {
  __mc_machine_runtime_send_u64(rt._raw, dst, kind, payload0, payload1)
}

@[public]
fn request(
  rt: Runtime,
  src: u64,
  dst: u64,
  kind: u64,
  payload0: u64,
  payload1: u64,
) -> u64 {
  __mc_machine_runtime_request_u64(rt._raw, src, dst, kind, payload0, payload1)
}

@[public]
fn send_reply(
  rt: Runtime,
  src: u64,
  reply_cap_id: u64,
  kind: u64,
  payload0: u64,
  payload1: u64,
) -> u64 {
  __mc_machine_runtime_reply_u64(rt._raw, src, reply_cap_id, kind, payload0, payload1)
}
