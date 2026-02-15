// Managed machine runtime wrappers.
//
// V1 goal: expose runtime operations to Machina code without leaking runtime
// struct layouts into user programs.

@[public]
type Runtime = {
  _raw: u64,
}

@[public]
type SpawnFailed = {}

@[public]
type StartFailed = {}

@[public]
type MachineUnknown = {}

@[public]
type MachineNotRunning = {}

@[public]
type MailboxFull = {}

@[public]
type RequestFailed = {}

@[public]
type ReplyCapUnknown = {}

@[public]
type ReplyDestUnknown = {}

@[public]
type ReplyDestNotRunning = {}

@[public]
type ReplyMailboxFull = {}

@[public]
type StepStatus = Idle | DidWork | Faulted

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
fn spawn(rt: Runtime, mailbox_cap: u64) -> u64 | SpawnFailed {
  let id = __mc_machine_runtime_spawn_u64(rt._raw, mailbox_cap);
  if id == 0 {
    SpawnFailed {}
  } else {
    id
  }
}

@[public]
fn start(rt: Runtime, machine_id: u64) -> () | StartFailed {
  if __mc_machine_runtime_start_u64(rt._raw, machine_id) == 0 {
    StartFailed {}
  } else {
    ()
  }
}

@[public]
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

@[public]
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

@[public]
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

@[public]
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
