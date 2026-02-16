// Managed machine runtime wrappers.
//
// V1 goal: expose runtime operations to Machina code without leaking runtime
// struct layouts into user programs.

@public
type Runtime = {
  _raw: u64,
}

@public
type SpawnFailed = {}

@public
type StartFailed = {}

@public
type MachineUnknown = {}

@public
type MachineNotRunning = {}

@public
type MailboxFull = {}

@public
type RequestFailed = {}

@public
type ReplyCapUnknown = {}

@public
type ReplyDestUnknown = {}

@public
type ReplyDestNotRunning = {}

@public
type ReplyMailboxFull = {}

@public
type StepStatus = Idle | DidWork | Faulted

@public
type BindDispatchFailed = {}

@public
type ManagedRuntimeUnavailable = {}

@public
fn new_runtime() -> Runtime {
  Runtime {
    _raw: __mc_machine_runtime_new(),
  }
}

@public
fn close_runtime(inout rt: Runtime) {
  __mc_machine_runtime_free(rt._raw);
  rt._raw = 0;
}

// Returns the process-managed runtime handle initialized by `@machines` main.
@public
fn managed_runtime() -> Runtime | ManagedRuntimeUnavailable {
  let raw = __mc_machine_runtime_managed_current_u64();
  if raw == 0 {
    ManagedRuntimeUnavailable {}
  } else {
    Runtime { _raw: raw }
  }
}

@public
fn spawn(rt: Runtime, mailbox_cap: u64) -> u64 | SpawnFailed {
  let id = __mc_machine_runtime_spawn_u64(rt._raw, mailbox_cap);
  if id == 0 {
    SpawnFailed {}
  } else {
    id
  }
}

@public
fn start(rt: Runtime, machine_id: u64) -> () | StartFailed {
  if __mc_machine_runtime_start_u64(rt._raw, machine_id) == 0 {
    StartFailed {}
  } else {
    ()
  }
}

@public
fn send(
  rt: Runtime,
  dst: u64,
  kind: u64,
  payload0: u64,
  payload1: u64,
) -> () | MachineUnknown | MachineNotRunning | MailboxFull {
  let status = __mc_machine_runtime_send_u64(rt._raw, dst, kind, payload0, payload1);
  if status == 0 {
    ()
  } else if status == 1 {
    MachineUnknown {}
  } else if status == 2 {
    MachineNotRunning {}
  } else {
    MailboxFull {}
  }
}

@public
fn request(
  rt: Runtime,
  src: u64,
  dst: u64,
  kind: u64,
  payload0: u64,
  payload1: u64,
) -> u64 | RequestFailed {
  let pending_id = __mc_machine_runtime_request_u64(rt._raw, src, dst, kind, payload0, payload1);
  if pending_id == 0 {
    // V1 bridge does not expose fine-grained request transport status.
    // Surface this as an explicit failure variant rather than silent zero.
    RequestFailed {}
  } else {
    pending_id
  }
}

@public
fn send_reply(
  rt: Runtime,
  src: u64,
  reply_cap_id: u64,
  kind: u64,
  payload0: u64,
  payload1: u64,
) -> () | ReplyCapUnknown | ReplyDestUnknown | ReplyDestNotRunning | ReplyMailboxFull {
  let status =
    __mc_machine_runtime_reply_u64(rt._raw, src, reply_cap_id, kind, payload0, payload1);
  if status == 0 {
    ()
  } else if status == 1 {
    ReplyCapUnknown {}
  } else if status == 2 {
    ReplyDestUnknown {}
  } else if status == 3 {
    ReplyDestNotRunning {}
  } else {
    ReplyMailboxFull {}
  }
}

// Low-level bridge used by compiler-generated managed bootstrap paths.
// `dispatch_fn` and `dispatch_ctx` are opaque runtime pointer words.
@public
fn bind_dispatch(
  rt: Runtime,
  machine_id: u64,
  dispatch_fn: u64,
  dispatch_ctx: u64,
) -> () | BindDispatchFailed {
  if __mc_machine_runtime_bind_dispatch_u64(rt._raw, machine_id, dispatch_fn, dispatch_ctx) == 0 {
    BindDispatchFailed {}
  } else {
    ()
  }
}

// Registers a thunk id -> function pointer mapping in runtime global registry.
// Intended for compiler-generated bootstrap glue.
@public
fn register_thunk(thunk_id: u64, dispatch_fn: u64) {
  __mc_machine_runtime_register_thunk_u64(thunk_id, dispatch_fn);
}

// Binds a machine slot to a previously-registered thunk id.
@public
fn bind_dispatch_thunk(
  rt: Runtime,
  machine_id: u64,
  thunk_id: u64,
  dispatch_ctx: u64,
) -> () | BindDispatchFailed {
  if __mc_machine_runtime_bind_dispatch_thunk_u64(rt._raw, machine_id, thunk_id, dispatch_ctx) == 0 {
    BindDispatchFailed {}
  } else {
    ()
  }
}

// Binds descriptor-driven dispatch to machine slot.
// `descriptor_id` is assigned at bootstrap registration time.
@public
fn bind_descriptor(
  rt: Runtime,
  machine_id: u64,
  descriptor_id: u64,
  initial_state_tag: u64,
) -> () | BindDispatchFailed {
  if __mc_machine_runtime_bind_descriptor_u64(rt._raw, machine_id, descriptor_id, initial_state_tag) == 0 {
    BindDispatchFailed {}
  } else {
    ()
  }
}

@public
fn step(rt: Runtime) -> StepStatus {
  let status = __mc_machine_runtime_step_u64(rt._raw);
  if status == 0 {
    StepStatus::Idle
  } else if status == 1 {
    StepStatus::DidWork
  } else {
    StepStatus::Faulted
  }
}
